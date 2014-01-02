{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module IRC
  ( -- * Connection management
    connect, reconnect
  , closeConnection, isOpenConnection
    -- ** Debugging
  , logC
  , getDebugOutput
    -- * Messages
  , send, receive
    -- ** Incoming messages
  , handleIncoming
    -- ** Specific messages
  , sendPing, sendPong
  , sendPrivMsg
  , sendJoin
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.Map as M
import           Data.Map (Map)
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

connect
  :: User -> Server -> [(Channel, Maybe Key)] -> IO (Maybe Connection)
connect usr srv channels = do

  -- create debugging output chan for connection
  debug_out <- newTChanIO

  -- see implementation of connect' below (-> Handling incoming messages)
  connect' usr srv (M.fromList channels) debug_out

-- | Try to reestablish an old (closed) connection
reconnect :: Connection -> IO (Maybe Connection)
reconnect con = do

  -- reuse server, user and debugging chan
  let usr       = con_user con
      srv       = con_server con
      chans     = con_channels con
      debug_out = con_debug_output con

  -- see implementation of connect' below (-> Handling incoming messages)
  connect' usr srv chans debug_out

-- | Send \"QUIT\" to server and close connection
closeConnection :: Connection -> Maybe ByteString -> IO ()
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
  userNickMsg = ircMsg "NICK" [ con_nick_cur con ] ""

sendUser :: Connection -> IO ()
sendUser con = do
  send con userMsg
 where
  usr     = con_user con
  userMsg = ircMsg "USER" [ usr_name usr
                          , "*", "*"
                          , usr_realname usr ] ""

sendPing :: Connection -> IO ()
sendPing con = send con $ ircMsg "PING" [] ""

sendPong :: Connection -> IO ()
sendPong con = send con $ ircMsg "PONG" [] ""

sendJoin :: Connection -> Channel -> Maybe Key -> IO Connection
sendJoin con chan mpw = do

  -- send JOIN request to server
  send con $ ircMsg "JOIN" (chan : maybe [] return mpw) ""

  -- Add channel + key to channels map and return new connection:
  return $ con { con_channels = M.insert chan mpw (con_channels con) }

sendPrivMsg
  :: Connection
  -> ByteString       -- ^ Target (user/channel)
  -> ByteString       -- ^ Text
  -> IO ()
sendPrivMsg con to txt = send con $ ircMsg "PRIVMSG" [to] txt

-- | Send "QUIT" command and closes connection to server. Do not re-use this
-- connection!
sendQuit :: Connection -> Maybe ByteString -> IO ()
sendQuit con mquitmsg = do
  send con quitMsg
 where
  quitMsg = ircMsg "QUIT" (maybe [] return mquitmsg) ""

--------------------------------------------------------------------------------
-- Handeling incoming messages

connect'
  :: User
  -> Server
  -> Map Channel (Maybe Key)
  -> TChan String
  -> IO (Maybe Connection)
connect' usr srv channels debug_out = handleExceptions $ do

  -- acquire IRC connection
  h <- connectTo (srv_host srv) (srv_port srv)
  hSetBuffering h LineBuffering
  hSetBinaryMode h False
  hSetEncoding h utf8

  -- connection is established at this point
  let connection = Connection usr (usr_nick usr)
                              srv channels M.empty h debug_out

  -- send username to IRC server
  sendUser connection
  sendNick connection
  waitForOK connection (usr_nick_alt usr)

  mapM_ (uncurry $ sendJoin connection) $ M.toList channels

  return $ Just connection

 where
  handleExceptions = handle $ \(_ :: IOException) -> return Nothing

  waitForOK con alt_nicks = do
    mmsg <- receive con
    case mmsg of

      -- check for authentication errors
      Right (msgCmd -> cmd)

        -- everything OK, we're done:
        | cmd == "001" -> return ()

        -- pick different nickname:
        | cmd `isError` err_NICKCOLLISION ||
          cmd `isError` err_NICKNAMEINUSE ->

          case alt_nicks of
            (alt:rst) -> do
              logC con $ "Notify (connect): Nickname changed to \""
                         ++ B8.unpack alt ++ "\"."
              -- use alternative nick names:
              let con' = con { con_nick_cur = alt }
              sendNick con'
              waitForOK con' rst

            [] -> do
              -- no alternative nicks!
              logC con "Error (connect): Nickname collision: \
                       \Try to supply a list of alternative nicknames. \
                       \(connection closed)"
              closeConnection con Nothing

      -- unknown message (e.g. NOTICE)
      Right msg -> do
        logC con $ "wait001: " ++ B8.unpack (fromIRCMsg msg)
        waitForOK con alt_nicks

      -- something went wrong
      Left  err -> do logC con $ "wait001: " ++ B8.unpack err
                      open <- isOpenConnection con
                      when open $ waitForOK con alt_nicks

  isError bs err = bs == (B8.pack $ show err)

-- | Handle incoming messages, change connection details if necessary
handleIncoming :: Connection -> IO Connection
handleIncoming con = do
  mmsg <- receive con
  case mmsg of

    -- parsing error
    Left err -> do
      logC con $ "Error (handleIncoming): Impossible parse: \""
                 ++ B8.unpack err ++ "\""
      return con

    Right msg@(msgCmd -> cmd)

      -- ping/pong game
      | cmd == "PING" -> do
        sendPong con
        return con

      -- join channels
      | cmd == "JOIN" -> do

        let channels = B8.split ',' $ msgTrail msg
        logC con $ "JOIN channels: " ++ concatMap B8.unpack channels

        -- add all channels to channel settings map and return new connection:
        return $
          foldr (\chan con' -> con' { con_channelsettings =
                                        M.insert chan
                                                 (ChannelSettings Nothing [])
                                                 (con_channelsettings con')
                                    })
                con      -- connection to update
                channels -- fold over channel list

      -- private messages
      | cmd == "PRIVMSG" -> do
        return con

      -- catch all unknown messages
      | otherwise -> do
        logC con $ "Warning (handleIncoming): Unknown message command: \""
                   ++ (init . init . B8.unpack $ fromIRCMsg msg) ++ "\""
        return con
