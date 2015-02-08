{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC.Connection
  ( ircConnect
  , UTCTime, IRCMsg
  ) where

import Control.Concurrent

import Network

import System.IO

-- time
import Data.Time

-- irc-bytestring
import Network.IRC.ByteString.Parser (IRCMsg, ircMsg, msgCmd)

-- lens
import Control.Lens hiding (Identity)
import Data.Text.Lens

-- local
import Network.IRC.Connection

import Irc2me.Backends.IRC.Helper
import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages

------------------------------------------------------------------------------
-- IRC broadcasting

ircConnect
  :: Server
  -> Identity
  -> ((UTCTime, IRCMsg) -> IO ())
  -> IO (Maybe IrcConnection)
ircConnect server ident broadcast

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
        -- error -> log error
        hPutStrLn stderr $
          "Error connecting to " ++ hostname ++ ":" ++ show port ++ " (" ++ show tlsSettings ++ "): "
          ++ show e
        return Nothing

      Right con -> do

        -- register user
        sendIrc con $ ircMsg "USER" [ nick, "*", "*", username ] ""
        sendIrc con $ ircMsg "NICK" [ nick ] ""

        tid <- forkIO $ handleIncoming con

        return $ Just $ IrcConnection
          { _ircThread   = tid
          , _ircIdentity = ident
          , _ircSend     = \cm -> sendIrc con (cm ^. from chatMessage)
          }

  | otherwise = do
    hPutStrLn stderr $ "Could not start IRC broadcast"
    return Nothing

 where
  -- handle incoming messages
  handleIncoming con = do
    mce <- handleIrcMessages con $ \tmsg@(_,msg) -> do

      if msgCmd msg == "PING" then
        sendIrc con $ ircMsg "PONG" [] ""
       else
        broadcast tmsg

    -- handle closed connections
    case mce of
      Nothing -> return ()
      Just ce -> do
        hPutStrLn stderr $ "Connection error: " ++ show ce
        closeConnection con
