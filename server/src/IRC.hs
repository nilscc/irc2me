{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module IRC
  ( -- * Connection management
    connect, reconnect
  , closeConnection, isOpenConnection
    -- ** Messages
    -- ** Debugging
  , getDebugOutput
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.ByteString as BL
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Attoparsec

import Network
import Network.IRC.ByteString.Parser

import System.IO

import IRC.ErrorCodes
import Types

--------------------------------------------------------------------------------
-- Debugging

logC :: Connection -> String -> IO ()
logC con s = do
  atomically $ writeTChan (con_debug_output con) s

getDebugOutput :: Connection -> IO (Maybe String)
getDebugOutput con = handleExceptions $ do
  s <- atomically $ readTChan (con_debug_output con)
  return $ Just s
 where
  handleExceptions = handle (\(_ :: BlockedIndefinitelyOnSTM) -> return Nothing)

--------------------------------------------------------------------------------
-- Connection management

connect :: User -> Server -> IO (Maybe Connection)
connect usr srv = do

  -- create debugging output chan for connection
  debug_out <- newTChanIO

  connect' usr srv debug_out

connect' :: User -> Server -> TChan String -> IO (Maybe Connection)
connect' usr srv debug_out = handleExceptions $ do

  -- acquire IRC connection
  h <- connectTo (srv_host srv) (srv_port srv)
  hSetBuffering h LineBuffering
  hSetBinaryMode h False
  hSetEncoding h utf8

  -- connection is established at this point
  let connection = Connection usr (usr_nick usr) (usr_nick_alt usr)
                              srv h debug_out

  -- send username to IRC server
  sendUser connection
  sendNick connection
  waitForOK connection

  return $ Just connection
 where
  handleExceptions = handle $ \(_ :: IOException) -> return Nothing
  waitForOK con = do
    mmsg <- receive con
    case mmsg of

      -- check for authentication errors
      Right (msgCmd -> cmd)

        -- everything OK, we're done:
        | cmd == "001" -> return ()

        -- pick different nickname:
        | cmd `isError` err_NICKCOLLISION ||
          cmd `isError` err_NICKNAMEINUSE ->

          case con_nick_alt con of
            (alt:rst) -> do
              logC con $ "Notify (connect): Nickname changed to \""
                         ++ alt ++ "\"."
              -- use alternative nick names:
              let con' = con { con_nick_cur = alt
                             , con_nick_alt = rst }
              sendNick con'
              waitForOK con'

            [] -> do
              -- no alternative nicks!
              logC con "Error (connect): Nickname collision: \
                       \Try to supply a list of alternative nicknames. \
                       \(connection closed)"
              closeConnection con Nothing

      -- unknown message (e.g. NOTICE)
      Right msg -> do
        logC con $ "wait001: " ++ B8.unpack (fromIRCMsg msg)
        waitForOK con

      -- something went wrong
      Left  err -> do logC con $ "wait001: " ++ B8.unpack err
                      open <- isOpenConnection con
                      when open $ waitForOK con

  isError bs err = bs == (B8.pack $ show err)

-- | Try to reestablish an old (closed) connection
reconnect :: Connection -> IO (Maybe Connection)
reconnect con = do

  -- reuse server, user and debugging chan
  let usr       = con_user con
      srv       = con_server con
      debug_out = con_debug_output con

  connect' usr srv debug_out

-- | Send \"QUIT\" to server and close connection
closeConnection :: Connection -> Maybe String -> IO ()
closeConnection con quitmsg = do
  open <- isOpenConnection con
  when open $ do
    sendQuit con quitmsg
    hClose (con_handle con)

isOpenConnection :: Connection -> IO Bool
isOpenConnection con = do
  hIsOpen (con_handle con)

--------------------------------------------------------------------------------
-- Sending & receiving

receive :: Connection -> IO (Either ByteString IRCMsg)
receive con = handleExceptions $ do
  bs <- BL.hGetLine (con_handle con)
  case toIRCMsg bs of
    Done _ msg -> return $ Right msg
    _          -> return $ Left bs
 where
  handleExceptions = handle $ \(e :: SomeException) -> do
                              hClose (con_handle con)
                              return $ Left $ B8.pack $ "Exception: " ++ show e
                                                      ++ " (connection closed)"

send :: Connection -> IRCMsg -> IO ()
send con msg = do
  open <- isOpenConnection con
  if open
     then BL.hPutStr (con_handle con) $ fromIRCMsg msg
     else logC con "Error (send): Connection to IRC server is closed!"

--------------------------------------------------------------------------------
-- Specific messages

sendNick :: Connection -> IO ()
sendNick con = do
  send con userNickMsg
 where
  userNickMsg = ircMsg "NICK" [ B8.pack $ con_nick_cur con ] ""

sendUser :: Connection -> IO ()
sendUser con = do
  send con userMsg
 where
  usr     = con_user con
  userMsg = ircMsg "USER" [ B8.pack $ usr_name usr
                          , "*", "*"
                          , B8.pack $ usr_realname usr ] ""

-- | Send "QUIT" command and closes connection to server. Do not re-use this
-- connection!
sendQuit :: Connection -> Maybe String -> IO ()
sendQuit con mquitmsg = do
  send con quitMsg
 where
  quitMsg = ircMsg "QUIT" (maybe [] (return . B8.pack) mquitmsg) ""
