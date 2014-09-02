{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.IRC where

import Control.Concurrent
import Control.Monad.Trans

import Data.ByteString (ByteString)
import Data.Time

import Network

-- irc-bytestring
import Network.IRC.ByteString.Parser

-- lens
import Control.Lens
import Data.Text.Lens

-- STM
import Control.Concurrent.STM

-- local
import Network.IRC.Connection

import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages

------------------------------------------------------------------------------
-- IRC broadcast data type

data IrcBroadcastStatus
  = IrcBroadcastConnecting
  | IrcBroadcastConnected
  | IrcBroadcastDisconnected (Maybe (ConnectionException IO))

data IrcBroadcast = IrcBroadcast
  { _broadcastIrcConnection   :: Connection IO
  , _messageBroadcast         :: TChan (UTCTime, IrcMessage)
  , _broadcastStatus          :: TVar  IrcBroadcastStatus
  }

newIrcBroadcast :: (Connection IO) -> IO IrcBroadcast
newIrcBroadcast con = do
  broadcastTChan <- newBroadcastTChanIO
  statusTVar     <- newTVarIO IrcBroadcastConnecting
  return $ IrcBroadcast
    { _broadcastIrcConnection = con
    , _messageBroadcast       = broadcastTChan
    , _broadcastStatus        = statusTVar
    }

--
-- Lenses
--

makePrisms ''IrcBroadcastStatus
makeLenses ''IrcBroadcast

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
        bc <- newIrcBroadcast con
        _  <- forkIO $ do
                ec <- ircMainBroadcast bc username nick
                stopBroadcasting bc ec
        return $ Just bc

  | otherwise = return Nothing

stopBroadcasting
  :: IrcBroadcast
  -> Maybe (ConnectionException IO)
  -> IO ()
stopBroadcasting bc ce = do

  atomically $ writeTVar statusTVar
                         (IrcBroadcastDisconnected ce)

  sendIrc con (ircMsg "QUIT" [] "")
  closeConnection con

 where
  con        = bc ^. broadcastIrcConnection
  statusTVar = bc ^. broadcastStatus

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
ircMainResponse bc (t,msg) = withMsg_ (t,msg)

  [ do command "001"
       liftIO $ atomically $ writeTVar (bc ^. broadcastStatus)
                              IrcBroadcastConnected

  , do command "PING"
       sendIrcT con $ ircMsg "PONG" [] ""

  , do -- everything else, broadcast to clients
       liftIO $ atomically $
         writeTChan (bc ^. messageBroadcast)
                    (t, msg ^. ircMessage)
  ]

 where
  con = bc ^. broadcastIrcConnection
