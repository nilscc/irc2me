{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

module IRC.Messages where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import qualified Data.Map as M
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Attoparsec

import Network.IRC.ByteString.Parser
import Network.TLS hiding (Key)

import System.IO

import IRC.Debug
import IRC.Types

--------------------------------------------------------------------------------
-- Helper

getTlsContext :: Connection -> IO (Maybe (Context, TLSBuffer, ThreadId))
getTlsContext con = atomically $ readTVar (con_tls_context con)

getCurrentNick :: Connection -> IO ByteString
getCurrentNick con = atomically $ getCurrentNick' con

getCurrentNick' :: Connection -> STM ByteString
getCurrentNick' con = readTVar (con_nick_cur con)

-- | Send \"QUIT\" to server and close connection
closeConnection :: Connection -> IO ()
closeConnection con = do
  handleException $ sendBye con
  handleException $ hClose (con_handle con)
  atomically $ writeTVar (con_status con) ConnectionClosed
 where
  handleException = handle (\(_ :: IOException) -> return ())

--------------------------------------------------------------------------------
-- Sending & receiving

receive :: Connection -> IO (Either ByteString IRCMsg)
receive con = handleExceptions $ do
  bs <- tlsGetLine con
  case toIRCMsg bs of
    Done _ msg -> return $ Right msg
    Partial f  -> case f "" of
                    Done _ msg -> return $ Right msg
                    _          -> return $ Left $ impossibleParseError bs
    _          -> return $ Left $ impossibleParseError bs
 where
  handleExceptions = handle (\(e :: IOException)              -> onExc e)
                   . handle (\(e :: BlockedIndefinitelyOnSTM) -> onExc e)
  onExc e = do
    closeConnection con
    return $ Left $ "Exception (receive): "
                    `BS.append` B8.pack (show e)
                    `BS.append` " (connection closed)"
  impossibleParseError bs =
    "Error (receive): Impossible parse: \"" `BS.append` bs `BS.append` "\""

send :: Connection -> IRCMsg -> IO ()
send con msg = handleExceptions $ do
  tlsSend con $ fromIRCMsg msg
 where
  handleExceptions =
    handle (\(_ :: IOException) -> logE con "send" "Is the connection open?")

sendRaw :: Connection -> ByteString -> IO ()
sendRaw con bs = handleExceptions $ do
  B8.hPutStr (con_handle con) bs
 where
  handleExceptions =
    handle (\(_ :: IOException) -> logE con "sendRaw" "Is the connection open?")

--------------------------------------------------------------------------------
-- SSL/TLS

tlsGetLine :: Connection -> IO ByteString
tlsGetLine con = do
  tls_cont <- getTlsContext con
  case tls_cont of
    Nothing -> BS.hGetLine (con_handle con)
    Just (_, buff, _) -> do
      atomically $ do
        bs <- readTVar buff `orElse` retry
        case B8.span (/= '\r') bs of
          (line, bs1)
            | Just ('\r', bs2)  <- B8.uncons bs1
            , Just ('\n', rest) <- B8.uncons bs2 -> do
              writeTVar buff rest
              return $ line `B8.append` "\r\n"
          _ -> retry

tlsSend :: Connection -> ByteString -> IO ()
tlsSend con bs = do
  tls_cont <- getTlsContext con
  case tls_cont of
    Just (ctxt, _, _) -> sendData ctxt (BL.fromStrict bs)
    Nothing           -> BS.hPutStr (con_handle con) bs

sendSTARTTLS :: Connection -> IO ()
sendSTARTTLS con = sendRaw con "STARTTLS\r\n"

sendCAPLS :: Connection -> IO ()
sendCAPLS con = sendRaw con "CAP LS\r\n"

sendCAPEnd :: Connection -> IO ()
sendCAPEnd con = sendRaw con "CAP END\r\n"

-- | Close a TLS session. May fail is handle is already closed!
sendBye :: Connection -> IO ()
sendBye con = do
  tls_cont <- getTlsContext con
  case tls_cont of
    Nothing -> return ()
    Just (ctxt,_,tid) -> do
      bye ctxt
      killThread tid

--------------------------------------------------------------------------------
-- Specific messages

sendNick :: Connection -> ByteString -> IO ()
sendNick con nick = do
  send con userNickMsg
 where
  userNickMsg = ircMsg "NICK" [ nick ] ""

sendUser :: Connection -> IO ()
sendUser con = do
  send con userMsg
 where
  usr     = con_user con
  userMsg = ircMsg "USER" [ usr_name usr
                          , "*", "*"
                          , usr_realname usr ] ""

sendUserAuth :: Connection -> IO ()
sendUserAuth con = do
  sendUser con
  nick <- getCurrentNick con
  sendNick con nick

sendPing :: Connection -> IO ()
sendPing con = send con $ ircMsg "PING" [] ""

sendPong :: Connection -> IO ()
sendPong con = send con $ ircMsg "PONG" [] ""

sendJoin :: Connection -> Channel -> Maybe Key -> IO ()
sendJoin con chan mpw = do

  -- send JOIN request to server
  send con $ ircMsg "JOIN" (chan : maybe [] return mpw) ""

  -- Add channel + key to channels map and return new connection:
  atomically $modifyTVar (con_channels con) $
    M.insert chan mpw

sendPrivMsg
  :: Connection
  -> ByteString       -- ^ Target (user/channel)
  -> ByteString       -- ^ Text
  -> IO ()
sendPrivMsg con to txt = send con $ ircMsg "PRIVMSG" [to] txt

sendPart :: Connection -> Channel -> IO ()
sendPart con channel = send con $ ircMsg "PART" [channel] ""

-- | Send "QUIT" command and closes connection to server. Do not re-use this
-- connection!
sendQuit :: Connection -> Maybe ByteString -> IO ()
sendQuit con mquitmsg = do
  send con quitMsg
 where
  quitMsg = ircMsg "QUIT" (maybe [] return mquitmsg) ""

