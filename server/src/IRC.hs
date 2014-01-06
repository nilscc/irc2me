{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

-- | Handle IRC connections, send and respond to incoming messages
module IRC
  ( -- * Connection management
    connect, reconnect
  , closeConnection, isOpenConnection
  , waitForInitialization
    -- ** Queries
  , getCurrentNick, getCurrentNick'
    -- ** Debugging
  , logE, logW, logI
  , getDebugOutput
    -- * Messages
  , send, receive
    -- ** Incoming messages
  , handleIncoming
  , getIncomingMessage
    -- ** Specific messages
  , sendPing, sendPong
  , sendPrivMsg
  , sendJoin
  , sendPart
  , sendQuit
  , sendNick
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Applicative

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Time

import Network
import Network.IRC.ByteString.Parser

import System.IO

import IRC.Codes
import IRC.Connection
import IRC.Debug
import IRC.Messages
import IRC.Types
import IRC.TLS

--------------------------------------------------------------------------------
-- Connection management

connect
  :: Server
  -> TLSSettings
  -> User
  -> IO (Maybe Connection)
connect srv tls_set usr = do

  -- create debugging output chan for connection
  debug_out <- newTChanIO

  -- see implementation of connect' below (-> Handling incoming messages)
  connect' srv tls_set usr M.empty debug_out

-- | Try to reestablish an old (closed) connection
reconnect :: Connection -> IO (Maybe Connection)
reconnect con = do

  -- just to make sure..
  is_open <- isOpenConnection con
  when is_open $ do
    logW con "reconnect" "Connection still open (connection closed)"
    send con $ quitMsg Nothing
    closeConnection con

  -- reuse server, user and debugging chan
  let usr       = con_user con
      srv       = con_server con
      tls_set   = con_tls_settings con
      debug_out = con_debug_output con

  chans <- getChannels con

  -- see implementation of connect' below (-> Handling incoming messages)
  connect' srv tls_set usr chans debug_out

--------------------------------------------------------------------------------
-- Handeling incoming messages

getIncomingMessage :: Connection -> IO (Maybe (UTCTime, Message))
getIncomingMessage con = handleExceptions $
  Just `fmap` atomically (readTChan (con_messages con))
 where
  handleExceptions = handle (\(_ :: BlockedIndefinitelyOnSTM) -> return Nothing)

connect'
  :: Server
  -> TLSSettings
  -> User
  -> Map Channel (Maybe Key)
  -> TChan String
  -> IO (Maybe Connection)
connect' srv tls_set usr channels debug_out = handleExceptions $ do

  -- acquire IRC connection
  h <- connectTo (srv_host srv) (srv_port srv)
  hSetBuffering h LineBuffering
  hSetBinaryMode h True

  -- prepare connection state variables
  nick_tvar     <- newTVarIO $ usr_nick usr
  chans_tvar    <- newTVarIO channels
  stat_tvar     <- newTVarIO ConnectionInitializing
  tls_tvar      <- newTVarIO Nothing
  msg_chan      <- newTChanIO

  -- connection is established at this point
  let con = Connection usr srv tls_set
                       nick_tvar
                       chans_tvar
                       h
                       stat_tvar
                       tls_tvar
                       msg_chan
                       debug_out

  -- initialize in background:
  void $ forkIO $ do
    -- check TLS settings
    resend <- initTLS con tls_set

    -- send username to IRC server
    waitForOK con (usr_nick_alt usr) resend

    mapM_ (uncurry $ sendJoin con) $ M.toList channels

    atomically $ do
      is_init <- isInitConnection' con
      when is_init $ setConnectionStatus' con ConnectionEstablished

  return $ Just con
      
 where
  handleExceptions = handle $ \(e :: IOException) -> do
    hPutStrLn stderr $ "IOException on connect: " ++ show e
    return Nothing

  waitForOK con alt_nicks resend = do

    -- resend unhandled messages from failed TLS initialization:
    let resend_rest | Just (_:r) <- resend = Just r
                    | otherwise            = Nothing
        receive' | Just (m:_) <- resend = return $ Right m
                 | otherwise            = receive con
    mmsg <- receive'

    case mmsg of

      -- check for authentication errors
      Right (time, msg@(msgCmd -> cmd))

        -- everything OK, we're done:
        | cmd == "001" -> return ()

        -- pick different nickname:
        | cmd `isError` err_NICKCOLLISION ||
          cmd `isError` err_NICKNAMEINUSE ->

          case alt_nicks of
            (alt:rst) -> do

              -- use alternative nick names:
              logI con "connect" $ "Nickname changed to \""
                                   ++ B8.unpack alt ++ "\"."
              setNick con alt
              sendNick con alt
              waitForOK con rst resend_rest

            [] -> do

              -- no alternative nicks!
              logE con "connect"
                       "Nickname collision: \
                       \Try to supply a list of alternative nicknames. \
                       \(connection closed)"
              sendQuit con Nothing
              closeConnection con

        | cmd == "NOTICE" -> do

          let (to:_)    = msgParams msg
              Just from = msgPrefix msg
              txt       = msgTrail msg
        
          addMessage con time $ NoticeMsg from to txt
          waitForOK con alt_nicks resend_rest

        -- unknown message (e.g. NOTICE) TODO: handle MOTD & other
        | otherwise -> do

          addMessage con time $ OtherMsg (msgPrefix msg)
                                    (msgCmd msg)
                                    (msgParams msg)
                                    (msgTrail msg)

          waitForOK con alt_nicks resend_rest

      -- something went wrong
      Left  err -> do logE con "connect" (B8.unpack err)
                      is_open <- isOpenConnection con
                      if is_open
                        then waitForOK con alt_nicks resend_rest
                        else return ()

  isError bs err = bs == (B8.pack err)

-- | Handle incoming messages, change connection details if necessary
handleIncoming :: Connection -> IO ()
handleIncoming con = handleRecvExceptions $ do

  mmsg <- receive con

  handleMsgExceptions mmsg $ case mmsg of

    -- parsing error
    Left err -> do

      logE con "handleIncoming" $ B8.unpack err

    Right (time, msg@(msgCmd -> cmd))

      --
      -- Commands
      --

      -- ping/pong game
      | cmd == "PING" -> do

        sendPong con

      -- join channels
      | cmd == "JOIN" -> do

        let (trail:_)       = B8.words $ msgTrail msg
            channels        = B8.split ',' trail
            Just (Left who) = msgPrefix msg

        is_cur <- isCurrentUser con who

        forM_ channels $ \chan ->
          addMessage con time $
            JoinMsg chan (if is_cur then Nothing else Just who)

        -- check if we joined a channel or if somebody else joined
        if is_cur
          then addChannels con channels
          else addUser con who channels

      | cmd == "PART" -> do

        let Just (Left who@UserInfo { userNick }) = msgPrefix msg
            (chan:_) = msgParams msg

        is_cur <- isCurrentUser con who
        -- send part message
        addMessage con time $
          PartMsg chan (if is_cur then Nothing else Just who)

        if is_cur
          then leaveChannel con chan
          else removeUser con chan userNick

      | cmd == "QUIT" -> do

        let Just (Left who@UserInfo { userNick }) = msgPrefix msg
            comment = msgTrail msg

        (chans, is_cur) <- atomically $
          (,) <$> getChannelsWithUser' con userNick
              <*> isCurrentUser' con who

        -- send part message
        addMessage con time $
          QuitMsg chans (if is_cur          then Nothing else Just who)
                        (if BS.null comment then Nothing else Just comment)

        if is_cur
          then closeConnection con
          else userQuit con chans userNick

      | cmd == "KICK" -> do

        let (chan:who:_) = msgParams msg
            comment      = msgTrail msg

        -- send kick message
        is_cur <- isCurrentNick con who
        addMessage con time $
          KickMsg chan (if is_cur          then Nothing else Just who)
                       (if BS.null comment then Nothing else Just comment)

        if is_cur
          then leaveChannel con chan
          else removeUser con chan who

      | cmd == "KILL" -> do

        closeConnection con

      -- private messages
      | cmd == "PRIVMSG" -> do

        let (to:_)    = msgParams msg
            Just from = msgPrefix msg
            txt       = msgTrail msg
        
        addMessage con time $ PrivMsg from to txt

      -- notice messages
      | cmd == "NOTICE" -> do

        let (to:_)    = msgParams msg
            Just from = msgPrefix msg
            txt       = msgTrail msg
        
        addMessage con time $ NoticeMsg from to txt

      -- nick changes
      | cmd == "NICK" -> do

        let (new:_)         = msgParams msg
            Just (Left who) = msgPrefix msg

        is_cur <- isCurrentUser con who
        addMessage con time $
          NickMsg (if is_cur then Nothing else Just who) new

        changeNickname con who new

      | cmd == "ERROR" -> do

        let err = msgTrail msg
        logE con "handleIncoming" $ "ERROR: \"" ++ B8.unpack err
                                                ++ "\" (connection closed)"
        closeConnection con

      --
      -- REPLIES
      --

      -- message of the day (MOTD)
      | cmd `isCode` rpl_MOTDSTART ||
        cmd `isCode` rpl_MOTD      -> do

        addMessage con time $ MOTDMsg (msgTrail msg)

      -- end of MOTD
      | cmd `isCode` rpl_ENDOFMOTD -> do

        -- do nothing
        return ()

      -- topic reply
      | cmd `isCode` rpl_TOPIC -> do

        let chan  = head $ msgParams msg
            topic = msgTrail msg

        -- TODO: send TOPIC message
        logI con "handleIncoming" $
                 "TOPIC for " ++ B8.unpack chan ++ ": " ++ B8.unpack topic

        setTopic con chan (Just topic)

      -- remove old topic (if any)
      | cmd `isCode` rpl_NOTOPIC -> do

        let chan = head $ msgParams msg

        -- TODO: send NOTOPIC message
        logI con "handleIncoming" $
                 "NOTOPIC for " ++ B8.unpack chan ++ "."

        setTopic con chan Nothing

      -- channel names
      | cmd `isCode` rpl_NAMREPLY -> do

        let (_:_:chan:_)   = msgParams msg
            namesWithFlags = map getUserflag $ B8.words $ msgTrail msg

        addMessage con time $ OtherMsg (msgPrefix msg)
                                  "NAMREPLY"
                                  (msgParams msg)
                                  (msgTrail msg)

        setChanNames con chan namesWithFlags

      -- end of channel names
      | cmd `isCode` rpl_ENDOFNAMES -> do

        return () -- do nothing

      --
      -- ERROR codes
      --

      -- nick name change failure
      | cmd `isCode` err_NICKCOLLISION ||
        cmd `isCode` err_NICKNAMEINUSE -> do

        addMessage con time $ ErrorMsg cmd

      --
      -- Other
      --

      | otherwise -> do

        addMessage con time $ OtherMsg (msgPrefix msg)
                                  (msgCmd msg)
                                  (msgParams msg)
                                  (msgTrail msg)

 where

  handleRecvExceptions =
    handle (\(e :: IOException) -> do
             logE con "handleIncoming" $
                      "IO exception: " ++ show e ++ " (connection closed)"
             closeConnection con)
  handleMsgExceptions mmsg =
    handle (\(_ :: PatternMatchFail) -> do
             logE con "handleIncoming" $
                      "Pattern match failure: " ++ show mmsg)

  isCode bs code = bs == (B8.pack code)
