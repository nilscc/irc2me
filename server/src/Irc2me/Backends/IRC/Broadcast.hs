{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC.Broadcast
  ( startBroadcasting, stopBroadcasting
  , IrcBroadcast
  , subscribe
  , broadcastNetworkId, NetworkID
  , sendIrcMessage
    -- * Connection status
  , IrcConnectionStatus (..)
  , getBroadcastConnectionStatus
  , isConnected
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception

import Control.Monad
import Control.Monad.Trans

import Data.Function

import Network

import System.IO

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

import Irc2me.Database.Tables.Networks (NetworkID)

import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages

------------------------------------------------------------------------------
-- IRC context

data IrcConnectionStatus
  = IrcConnecting ByteString      -- ^ Current nick being used for
                                  -- authentication
  | IrcConnected
  | IrcDisconnected (Maybe (ConnectionException IO))

--
-- Lenses
--

makePrisms ''IrcConnectionStatus

------------------------------------------------------------------------------
-- IRC broadcast data type

data IrcBroadcast = IrcBroadcast
  { _broadcastIrcConnection   :: Connection IO
  , _broadcastNetworkId       :: NetworkID
  , _broadcastMessages        :: TChan (UTCTime, IrcMessage)
  , _broadcastIrcStatus       :: TVar  IrcConnectionStatus
  , _broadcastThread          :: Maybe ThreadId
  }

--
-- Lenses
--

makeLenses ''IrcBroadcast

--
-- Utility functions
--

newIrcBroadcast :: NetworkID -> (Connection IO) -> IO IrcBroadcast
newIrcBroadcast nid con = do
  broadcastTChan <- newBroadcastTChanIO
  statusTVar     <- newTVarIO (IrcDisconnected Nothing)
  return $ IrcBroadcast
    { _broadcastIrcConnection = con
    , _broadcastNetworkId     = nid
    , _broadcastMessages      = broadcastTChan
    , _broadcastIrcStatus     = statusTVar
    , _broadcastThread        = Nothing
    }

getBroadcastConnectionStatus
  :: MonadIO m => IrcBroadcast -> m IrcConnectionStatus
getBroadcastConnectionStatus bc =

  liftIO $ atomically $ readTVar (bc ^. broadcastIrcStatus)

isConnected :: MonadIO m => IrcBroadcast -> m Bool
isConnected bc = do

  status <- getBroadcastConnectionStatus bc
  case status of
    IrcConnected -> return True
    _            -> return False

--
-- Sending messages
--

sendIrcMessage :: MonadIO m => IrcBroadcast -> IrcMessage -> m ()
sendIrcMessage bc msg = liftIO $ do
  sendIrc (bc ^. broadcastIrcConnection) (msg ^. from ircMessage)

  -- re-broadcast, FIXME
  now <- getCurrentTime
  broadcast bc (now, msg ^. from ircMessage)

--
-- Subscription
--

broadcast :: MonadIO m => IrcBroadcast -> (UTCTime, IRCMsg) -> m ()
broadcast bc (t,msg) = liftIO $ atomically $
  writeTChan (bc ^. broadcastMessages) (t, msg ^. ircMessage)

subscribe :: IrcBroadcast -> (NetworkID -> (UTCTime, IrcMessage) -> IO ()) -> IO ()
subscribe bc go = do

  incoming <- atomically $ dupTChan (bc ^. broadcastMessages)

  fix $ \loop -> do

    m <- atomically $ do
      (Just <$> readTChan incoming) <|> do
        status <- readTVar (bc ^. broadcastIrcStatus)
        case status of
          IrcConnecting _   -> retry
          IrcConnected      -> retry
          IrcDisconnected _ -> return Nothing

    case m of
      Nothing  -> return ()
      Just msg -> do
        go (bc ^. broadcastNetworkId) msg
        loop

--
-- Guards
--

connecting :: (MonadIO m, MonadPlus m) => IrcBroadcast -> m ByteString
connecting bc = do

  status <- liftIO $ atomically $ readTVar (bc ^. broadcastIrcStatus)
  case status of
    IrcConnecting n -> return n
    _               -> mzero

------------------------------------------------------------------------------
-- IRC broadcasting

startBroadcasting :: IrcIdentity -> (NetworkID, IrcServer) -> IO (Maybe IrcBroadcast)
startBroadcasting ident (nid,server)

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
      Left e  -> do
        hPutStrLn stderr $
          "Error connecting to " ++ hostname ++ ":" ++ show port ++ " (" ++ show tlsSettings ++ "): "
          ++ show e
        return Nothing
      Right con -> do

        -- create new broadcast
        bc <- newIrcBroadcast nid con

        -- set status to 'connecting'
        atomically $ writeTVar (bc ^. broadcastIrcStatus)
                               (IrcConnecting nick)

        -- start broadcasting in new thread
        t <- forkIO $ do
               onException
                 (do ec <- ircMainBroadcast bc username nick
                     stopBroadcasting bc ec
                 )
                 (stopBroadcasting bc Nothing)

        return $ Just (bc & broadcastThread .~ Just t)

  | otherwise = return Nothing

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

      atomically $ writeTVar statusTVar $ IrcDisconnected ce

 where
  con        = bc ^. broadcastIrcConnection
  statusTVar = bc ^. broadcastIrcStatus

--
-- IRC event handler
--

ircMainBroadcast
  :: IrcBroadcast
  -> ByteString   -- ^ username
  -> ByteString   -- ^ nickname
  -> IO (Maybe (ConnectionException IO))
ircMainBroadcast bc username nickname = do

  let con = bc ^. broadcastIrcConnection

  -- register user
  sendIrc con $ ircMsg "USER" [ nickname, "*", "*", username ] ""
  sendIrc con $ ircMsg "NICK" [ nickname ] ""

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
       nick <- connecting bc

       -- broadcast all messages while still connecting
       broadcast bc tmsg

       void . optional $ msum
         [ do -- everything's ok, switch to 'Connected'
              command "001"
              liftIO . atomically $
                writeTVar (bc ^. broadcastIrcStatus) IrcConnected

         , do -- change nickname if necessary
              command err_NICKNAMEINUSE

              let nick' = nick `B8.append` "_"

              -- store new nick
              liftIO . atomically $
                writeTVar (bc ^. broadcastIrcStatus) (IrcConnecting nick')

              -- send new nick to server
              sendIrcT con $ ircMsg "NICK" [ nick' ] ""
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
