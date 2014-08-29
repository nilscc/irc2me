{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

module IRC.Messages where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens.Operators

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Attoparsec
import           Data.Time

import Network.IRC.ByteString.Parser
import Network.TLS hiding (Key)

import System.IO

import IRC.Debug
import IRC.Types

--------------------------------------------------------------------------------
-- Helper

getTlsContext :: Connection -> IO (Maybe (Context, TLSBuffer, ThreadId))
getTlsContext con = atomically $ readTVar (con ^. con_tls_context)

getCurrentNick :: Connection -> IO ByteString
getCurrentNick con = atomically $ getCurrentNick' con

getCurrentNick' :: Connection -> STM ByteString
getCurrentNick' con = readTVar (con ^. con_nick_cur)

-- | Send \"QUIT\" to server and close connection
closeConnection :: Connection -> IO ()
closeConnection con = do
  handleException $ sendBye con
  handleException $ hClose (con ^. con_handle)
  atomically $ writeTVar (con ^. con_status) ConnectionClosed
 where
  handleException = handle (\(_ :: IOException) -> return ())

--------------------------------------------------------------------------------
-- Sending & receiving

receive :: Connection -> IO (Either ByteString (UTCTime, IRCMsg))
receive con = handleExceptions $ do
  bs <- tlsGetLine con
  now <- getCurrentTime
  case toIRCMsg bs of
    Done _ msg -> return $ Right (now, msg)
    Partial f  -> case f "" of
                    Done _ msg -> return $ Right (now, msg)
                    _          -> return $ Left $ impossibleParseError bs
    _          -> return $ Left $ impossibleParseError bs
 where
  handleExceptions = handle (\(e :: IOException)              -> onExc e)
                   . handle (\(e :: BlockedIndefinitelyOnSTM) -> onExc e)
  onExc e = do
    closeConnection con
    return $ Left $ B8.pack (show e) `BS.append` " (connection closed)"

  impossibleParseError bs =
    "Impossible parse: \"" `BS.append` bs `BS.append` "\""

send :: Connection -> IRCMsg -> IO ()
send con msg = handleExceptions $ do
  tlsSend con $ fromIRCMsg msg
 where
  handleExceptions =
    handle (\(_ :: IOException) -> logE con "send" "Is the connection open?")

sendRaw :: Connection -> ByteString -> IO ()
sendRaw con bs = handleExceptions $ do
  B8.hPutStr (con ^. con_handle) bs
 where
  handleExceptions =
    handle (\(_ :: IOException) -> logE con "sendRaw" "Is the connection open?")

--------------------------------------------------------------------------------
-- SSL/TLS

tlsGetLine :: Connection -> IO ByteString
tlsGetLine con = do
  tls_cont <- getTlsContext con
  case tls_cont of
    Nothing -> BS.hGetLine (con ^. con_handle)
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
    Nothing           -> BS.hPutStr (con ^. con_handle) bs

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

nickMsg :: ByteString -> IRCMsg
nickMsg nick = ircMsg "NICK" [ nick ] ""

userMsg :: Identity -> IRCMsg
userMsg usr = ircMsg "USER" [ usr ^. ident_name
                            , "*", "*"
                            , usr ^. ident_realname ] ""

pingMsg :: IRCMsg
pingMsg = ircMsg "PING" [] ""

pongMsg :: ByteString -> IRCMsg
pongMsg trail = ircMsg "PONG" [] trail

joinMsg :: Channel -> Maybe Key -> IRCMsg
joinMsg chan mpw = ircMsg "JOIN" (chan : maybe [] return mpw) ""

  {- FIXME: store channel pw

  -- Add channel + key to channels map and return new connection:
  atomically $modifyTVar (con_channels con) $
    M.insert chan mpw

  -}

privMsg
  :: ByteString       -- ^ Target (user/channel)
  -> ByteString       -- ^ Text
  -> IRCMsg
privMsg to txt = ircMsg "PRIVMSG" [to] txt

partMsg :: Channel -> IRCMsg
partMsg channel = ircMsg "PART" [channel] ""

quitMsg :: Maybe ByteString -> IRCMsg
quitMsg mquitmsg = ircMsg "QUIT" (maybe [] return mquitmsg) ""

-- | Send both user and nick name from current connection to server
sendUserAuth :: Connection -> IO ()
sendUserAuth con = do
  send con $ userMsg (con ^. con_user)
  nick <- getCurrentNick con
  send con $ nickMsg nick
