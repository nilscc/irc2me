{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC.Connection
  ( ircConnect
  , UTCTime, IRCMsg
  ) where

import Control.Concurrent
import Control.Concurrent.Broadcast
import Control.Concurrent.STM
import Control.Monad.Trans

import Network

import System.IO

-- time
import Data.Time

-- irc-bytestring
import Network.IRC.ByteString.Parser (IRCMsg, ircMsg, msgCmd)

-- lens
import Control.Lens hiding (Identity)
--import Data.Text.Lens

-- local
import Network.IRC.Connection

import Irc2me.Backends.IRC.Helper
import Irc2me.Backends.IRC.Types
-- import Irc2me.Frontend.Messages.Helper
-- import Irc2me.Frontend.Messages

------------------------------------------------------------------------------
-- IRC broadcasting

ircConnect
  :: Server
  -> Identity
  -> IO (Maybe IrcConnection)
ircConnect server ident

    -- server
  | hostname <- server ^. serverHost -- . _Just . _Text
  , port     <- server ^. serverPort

    -- identity
  , nick     <- ident ^. identityNick . from encoded -- . _Just . from encoded
  , username <- ident ^. identityName . from encoded -- . _Just . from encoded

  = do

    let tlsSettings = if server ^. serverUseTLS
                        then TLS
                        else Plaintext -- FIXME: Use OptionalTLS

    mcon <- connect tlsSettings hostname (PortNumber $ fromIntegral port)
    case mcon of

      Left e  -> do
        -- error -> log error
        hPutStrLn stderr $
          "Error connecting to " ++ hostname ++ ":" ++ show port ++ " (" ++ show tlsSettings ++ "): "
          ++ show e
        return Nothing

      Right con -> do

        -- register user
        sendIrc con $ ircMsg "USER" [ nick, "*", "*", username ] ""
        sendIrc con $ ircMsg "NICK" [ nick ] ""

        -- start new broadcast thread for IRC connection
        bc <- startBroadcasting $ handleIncoming con

        return $ Just $ IrcConnection
          { _ircSend      = sendIrc con
          , _ircBroadcast = bc
          }

  | otherwise = do
    hPutStrLn stderr $ "Could not start IRC broadcast"
    return Nothing

 where
  -- handle incoming messages
  handleIncoming con = do
    bcf <- getBroadcastFunction
    mce <- liftIO $ handleIrcMessages con $ \tmsg@(_,msg) -> do

      if msgCmd msg == "PING" then
        liftIO $ sendIrc con $ ircMsg "PONG" [] ""
       else
        bcf tmsg

    -- handle closed connections
    case mce of
      Nothing -> return ()
      Just ce -> liftIO $ do
        hPutStrLn stderr $ "Connection error: " ++ show ce
        closeConnection con
