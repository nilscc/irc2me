{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.IRC
  ( connectToIrc
  , IrcBroadcast, subscribe
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception

import Control.Monad
import Control.Monad.Trans

import Data.Function

import Network

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

-- time
import Data.Time

-- irc-bytestring
import Network.IRC.ByteString.Parser (IRCMsg, ircMsg)

-- lens
import Control.Lens
import Data.Text.Lens

-- STM
import Control.Concurrent.STM

-- local
import Network.IRC.Message.Codes
import Network.IRC.Connection

import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages

------------------------------------------------------------------------------
-- IRC context

data IrcConnectionStatus
  = IrcConnecting
  | IrcConnected
  | IrcDisconnected (Maybe (ConnectionException IO))

data IrcStatus = IrcStatus
  { _currentIrcConnectionStatus :: IrcConnectionStatus
  , _currentIrcNickname         :: ByteString
  }

--
-- Lenses
--

makeLenses ''IrcStatus
makePrisms ''IrcConnectionStatus

--
-- Utilities
--

emptyIrcStatus :: IrcStatus
emptyIrcStatus = IrcStatus
  { _currentIrcConnectionStatus = IrcDisconnected Nothing
  , _currentIrcNickname         = ""
  }

isConnecting :: IrcConnectionStatus -> Bool
isConnecting IrcConnecting = True
isConnecting _             = False

------------------------------------------------------------------------------
-- IRC broadcast data type

data IrcBroadcast = IrcBroadcast
  { _broadcastIrcConnection   :: Connection IO
  , _broadcastMessages        :: TChan (UTCTime, IrcMessage)
  , _broadcastIrcStatus       :: TVar  IrcStatus
  , _broadcastThread          :: Maybe ThreadId
  }

--
-- Lenses
--

makeLenses ''IrcBroadcast

--
-- Utility functions
--

newIrcBroadcast :: (Connection IO) -> IO IrcBroadcast
newIrcBroadcast con = do
  broadcastTChan <- newBroadcastTChanIO
  statusTVar     <- newTVarIO emptyIrcStatus
  return $ IrcBroadcast
    { _broadcastIrcConnection = con
    , _broadcastMessages      = broadcastTChan
    , _broadcastIrcStatus     = statusTVar
    , _broadcastThread        = Nothing
    }

stopBroadcasting
  :: IrcBroadcast
  -> Maybe (ConnectionException IO)
  -> IO ()
stopBroadcasting bc ce = do

  case bc ^. broadcastThread of
    Just tid ->

      -- outside of broadcasting thread
      killThread tid

    Nothing  -> do

      -- within current broadcasting thread
      sendIrc con (ircMsg "QUIT" [] "Goodbye.")

      closeConnection con

      atomically $ do
        status <- readTVar statusTVar
        writeTVar statusTVar $ status
          & currentIrcConnectionStatus .~ IrcDisconnected ce

 where
  con        = bc ^. broadcastIrcConnection
  statusTVar = bc ^. broadcastIrcStatus

--
-- Subscription
--

broadcast :: MonadIO m => IrcBroadcast -> (UTCTime, IRCMsg) -> m ()
broadcast bc (t,msg) = liftIO $ atomically $
  writeTChan (bc ^. broadcastMessages) (t, msg ^. ircMessage)

subscribe :: IrcBroadcast -> ((UTCTime, IrcMessage) -> IO ()) -> IO ()
subscribe bc go = do

  incoming <- atomically $ dupTChan (bc ^. broadcastMessages)

  fix $ \loop -> do

    m <- atomically $ do
      (Just <$> readTChan incoming) <|> do
        status <- readTVar (bc ^. broadcastIrcStatus)
        case status ^. currentIrcConnectionStatus of
          IrcConnecting     -> retry
          IrcConnected      -> retry
          IrcDisconnected _ -> return Nothing

    case m of
      Nothing  -> return ()
      Just msg -> do
        go msg
        loop

--
-- IRC status changes
--

modifyIrcStatus :: IrcBroadcast -> (IrcStatus -> IrcStatus) -> STM IrcStatus
modifyIrcStatus bc f = do
  st <- readTVar (bc ^. broadcastIrcStatus)
  let st' = f st
  writeTVar (bc ^. broadcastIrcStatus) st'
  return st'

modifyIrcStatus_ :: IrcBroadcast -> (IrcStatus -> IrcStatus) -> STM ()
modifyIrcStatus_ bc f = () <$ modifyIrcStatus bc f

modifyIrcStatusIO_ :: MonadIO m => IrcBroadcast -> (IrcStatus -> IrcStatus) -> m ()
modifyIrcStatusIO_ bc f = liftIO . atomically $ modifyIrcStatus_ bc f

setNickname :: IrcBroadcast -> ByteString -> IO ()
setNickname bc nick = modifyNickname_ bc (const nick)

modifyNickname_ :: MonadIO m => IrcBroadcast -> (ByteString -> ByteString) -> m ()
modifyNickname_ bc f = liftIO $ do
  atomically $ modifyIrcStatus_ bc $ currentIrcNickname %~ f

sendCurrentNickname :: MonadIO m => IrcBroadcast -> m ()
sendCurrentNickname bc = liftIO $ do
  st <- atomically $ readTVar (bc ^. broadcastIrcStatus)
  sendIrc con $ ircMsg "NICK" [ st ^. currentIrcNickname ] ""
 where
  con   = bc ^. broadcastIrcConnection

--
-- Guards
--

connecting :: (MonadIO m, MonadPlus m) => IrcBroadcast -> m ()
connecting bc = do

  status <- liftIO $ atomically $ do
    view currentIrcConnectionStatus <$> readTVar (bc ^. broadcastIrcStatus)

  guard $ isConnecting status

------------------------------------------------------------------------------
-- IRC main loop

connectToIrc :: IrcIdentity -> IrcServer -> IO (Maybe IrcBroadcast)
connectToIrc ident server

    -- server
  | Just hostname <- server ^? serverHost . _Just . _Text
  , Just port     <- server ^. serverPort

    -- identity
  , Just nick     <- ident ^? identityNick . _Just . from encoded
  , Just username <- ident ^? identityName . _Just . from encoded

  = do

    let tlsSettings = if server ^. serverUseTLS . non False
                        then TLS
                        else Plaintext -- FIXME: Use OptionalTLS

    mcon <- connect tlsSettings hostname (PortNumber $ fromIntegral port)

    case mcon of
      Left  _e  -> return Nothing
      Right con -> do

        -- create new broadcast
        bc <- newIrcBroadcast con

        -- set status to 'connecting'
        atomically $ modifyIrcStatus_ bc $
          currentIrcConnectionStatus .~ IrcConnecting

        -- start broadcasting in new thread
        t <- forkIO $ do
               onException
                 (do ec <- ircMainBroadcast bc username nick
                     stopBroadcasting bc ec
                 )
                 (stopBroadcasting bc Nothing)

        return $ Just (bc & broadcastThread .~ Just t)

  | otherwise = return Nothing

ircMainBroadcast
  :: IrcBroadcast
  -> ByteString   -- ^ username
  -> ByteString   -- ^ nickname
  -> IO (Maybe (ConnectionException IO))
ircMainBroadcast bc username nickname = do

  let con = bc ^. broadcastIrcConnection

  setNickname bc nickname

  -- register user
  sendIrc con $ ircMsg "USER" [ nickname, "*", "*", username ] ""
  sendCurrentNickname bc

  ec <- handleIrcMessages con (ircMainResponse bc)

  -- dump parsing errors for debugging
  case ec of
    Just (ParsingError' _ hp) ->
      print =<< dumpHaltedProducer hp
    _ -> return ()

  return ec

ircMainResponse
  :: IrcBroadcast
  -> (UTCTime, IRCMsg)
  -> IO ()
ircMainResponse bc tmsg = withMsg_ tmsg

  [ do -- only fire while still connecting
       connecting bc

       -- broadcast all messages while still connecting
       broadcast bc tmsg

       msum [ do -- everything's ok, switch to 'Connected'
                 command "001"
                 modifyIrcStatusIO_ bc $
                   currentIrcConnectionStatus .~ IrcConnected
                 sendIrcT con $ ircMsg "JOIN" [ "#test" ] ""

            , do -- change nickname if necessary
                 command err_NICKNAMEINUSE
                 modifyNickname_ bc (`B8.append` "_")
                 sendCurrentNickname bc
            ]

  , do -- respond to PING with PONG
       command "PING"
       sendIrcT con $ ircMsg "PONG" [] ""

  -- FIXME: handle nickname changes

  , do -- broadcast everything else
       broadcast bc tmsg
  ]

 where
  con = bc ^. broadcastIrcConnection
