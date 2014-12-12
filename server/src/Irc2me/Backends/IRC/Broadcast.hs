{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC.Broadcast
  ( startIrcBroadcast
  , UTCTime, IRCMsg
  ) where

import Control.Applicative

import Network

import System.IO

-- pipes
import Pipes

-- time
import Data.Time

-- irc-bytestring
import Network.IRC.ByteString.Parser (IRCMsg, ircMsg)

-- lens
import Control.Lens hiding (Identity)
import Data.Text.Lens

-- local
import Control.Concurrent.Broadcast

--import Network.IRC.Message.Codes
import Network.IRC.Connection

import Irc2me.Backends.IRC.Helper
import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages

------------------------------------------------------------------------------
-- IRC broadcasting

startIrcBroadcast
  :: Server
  -> Identity
  -> ((UTCTime, IRCMsg) -> msg)  -- ^ Converter/evalutor for incoming messages (before
                      -- broadcast)
  -> IO (Maybe (Broadcast msg))
startIrcBroadcast server ident convert

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
        liftIO $ sendIrc con $ ircMsg "USER" [ nick, "*", "*", username ] ""
        liftIO $ sendIrc con $ ircMsg "NICK" [ nick ] ""

        -- create new broadcast
        Just <$> startBroadcasting' (ircBroadcast con)
                                    (stopIrcBroadcast con Nothing)

  | otherwise = return Nothing

 where
  ircBroadcast con = do

    -- start broadcasting messages
    bc <- getBroadcastFunction
    mce <- lift $ handleIrcMessages con $ \tmsg -> do

      -- output for debugging purposes
      putStrLn $ testFormat tmsg

      -- broadcast
      bc $ convert tmsg

    -- handle closed connections
    case mce of
      Nothing -> return ()
      Just ce -> liftIO $ stopIrcBroadcast con (Just ce)

stopIrcBroadcast
  :: Connection m
  -> Maybe (ConnectionException IO)
  -> IO ()
stopIrcBroadcast con_ ce = do

  -- 'unload' any (left over) producers
  let con = con_ { ircMessages = return ce }

  case ce of
    Just e  -> hPutStr stderr $ "Connection error: " ++ show e
    Nothing ->

      -- gracefully quit (no connection error!)
      sendIrc con (ircMsg "QUIT" [] "Goodbye.")

  closeConnection con
