{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

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
  , getIncoming
    -- ** Specific messages
  , pingMsg, pongMsg
  , privMsg
  , joinMsg
  , partMsg
  , quitMsg
  , nickMsg
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.ByteString (ByteString)
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
  -> IO (Maybe (Connection, [(UTCTime, Message)]))
connect srv tls_set usr = do

  -- create debugging output chan for connection
  debug_out <- newTChanIO

  -- see implementation of connect' below (-> Handling incoming messages)
  connect' srv tls_set usr M.empty debug_out

-- | Try to reestablish an old (closed) connection
reconnect :: Connection -> IO (Maybe (Connection, [(UTCTime, Message)]))
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

connect'
  :: Server
  -> TLSSettings
  -> User
  -> Map Channel (Maybe Key)
  -> TChan String
  -> IO (Maybe (Connection, [(UTCTime, Message)]))
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

  -- connection is established at this point
  let con = Connection usr srv tls_set
                       nick_tvar
                       chans_tvar
                       h
                       stat_tvar
                       tls_tvar
                       debug_out

  -- check TLS settings
  resend <- initTLS con tls_set

  -- send username to IRC server
  msgs <- waitForOK con (usr_nick_alt usr) resend []

  sequence_ [ send con $ joinMsg chan mkey
            | (chan, mkey) <- M.toList channels
            ]

  atomically $ do
    is_init <- isInitConnection' con
    when is_init $ setConnectionStatus' con ConnectionEstablished

  return $ Just (con, msgs)
      
 where
  handleExceptions = handle $ \(e :: IOException) -> do
    hPutStrLn stderr $ "IOException on connect: " ++ show e
    return Nothing

  waitForOK con alt_nicks resend msgs = requireInitConnection $ do

    -- resend unhandled messages from failed TLS initialization:
    let resend_rest | Just (_:r) <- resend = Just r
                    | otherwise            = Nothing
        receive' | Just (m:_) <- resend = return $ Right m
                 | otherwise            = receive con
    mmsg <- receive'

    case mmsg of

      -- check for authentication errors
      Right (time, msg) -> do
        stat <- initWaitForOK con msg alt_nicks []
        case stat of
          InitOK     -> return msgs
          InitCancel -> return msgs
          InitWaitForOK alt_nicks' msgs' ->
            waitForOK con alt_nicks' resend_rest (msgs ++ map (time,) msgs')

      -- something went wrong
      Left err -> do
        logE con "connect" (B8.unpack err)
        is_open <- isOpenConnection con
        if is_open
          then waitForOK con alt_nicks resend_rest msgs
          else return msgs
   where
    requireInitConnection run = do
      is_init <- isInitConnection con
      if is_init
        then run
        else do logE con "connect" "Connection closed."
                return msgs

data InitStatus
  = InitWaitForOK { _alt_nicks :: [ByteString]
                  , _init_msgs :: [Message]
                  }
  | InitOK
  | InitCancel

initWaitForOK
  :: Connection
  -> IRCMsg
  -> [ByteString]
  -> [Message]
  -> IO InitStatus
initWaitForOK con msg@(msgCmd -> cmd) alt_nicks msgs

  -- everything OK, we're done:
  | cmd == "001" = return InitOK

  -- pick different nickname:
  | cmd `isError` err_NICKCOLLISION ||
    cmd `isError` err_NICKNAMEINUSE =

    case alt_nicks of
      (alt:rst) -> do

        -- use alternative nick names:
        logI con "connect" $ "Nickname changed to \""
                             ++ B8.unpack alt ++ "\"."
        setNick con alt
        send con $ nickMsg alt
        return $ InitWaitForOK rst msgs

      [] -> do

        -- no alternative nicks!
        logE con "connect"
                 "Nickname collision: \
                 \Try to supply a list of alternative nicknames. \
                 \(connection closed)"
        send con $ quitMsg Nothing
        closeConnection con

        return InitCancel

  | cmd == "NOTICE" = do

    let (to:_)    = msgParams msg
        Just from = msgPrefix msg
        txt       = msgTrail msg
  
    return $ InitWaitForOK alt_nicks (msgs ++ [NoticeMsg from to txt])

  -- unknown message (e.g. NOTICE) TODO: handle MOTD & other
  | otherwise = do

    let other = OtherMsg (msgPrefix msg)
                         (msgCmd msg)
                         (msgParams msg)
                         (msgTrail msg)

    return $ InitWaitForOK alt_nicks (msgs ++ [other])
 where
  isError bs err = bs == (B8.pack err)

-- | Handle incoming messages, change connection details if necessary
getIncoming :: Connection -> IO (Maybe (UTCTime, Message))
getIncoming con = handleRecvExceptions $

  requireOpenConnection $ do

  mmsg <- receive con

  handleMsgExceptions mmsg $ case mmsg of

    Right (time, msg) -> runResult time $ handleIncoming msg

    -- parsing error
    Left err -> do
      logE con "getIncoming" $ B8.unpack err
      return Nothing

 where

  runResult time (IncomingReqUsr f) = runResult time $ f (con_user con)
  runResult time (IncomingReqNck f) = getCurrentNick con >>= runResult time . f
  runResult time (IncomingResult to_send res quit) = do
    mapM_ (send con) to_send
    onJust quit $ \reason -> do
      logI con "handleIncoming" $ reason ++ " (connection closed)"
      closeConnection con
    return $ (time,) `fmap` res

  onJust what = maybe (return ()) `flip` what

  requireOpenConnection run = do
    is_open <- isOpenConnection con
    if is_open
      then run
      else do logE con "getIncoming" "Connection closed."
              return Nothing

  handleRecvExceptions =
    handle (\(e :: IOException) -> do
             logE con "handleIncoming" $
                      "IO exception: " ++ show e ++ " (connection closed)"
             closeConnection con
             return Nothing)
  handleMsgExceptions mmsg =
    handle (\(_ :: PatternMatchFail) -> do
             logE con "handleIncoming" $
                      "Pattern match failure: " ++ show mmsg
             return Nothing)

type Reason = String

data IncomingResult
  = IncomingResult { send_msgs    :: [IRCMsg]
                   , add_msg      :: Maybe Message
                   , quit         :: Maybe Reason
                   }
  | IncomingReqUsr { _withUser     :: User -> IncomingResult }
  | IncomingReqNck { _withNick     :: Nickname -> IncomingResult }

-- | Testable interface
handleIncoming :: IRCMsg -> IncomingResult
handleIncoming msg@(msgCmd -> cmd)

  --
  -- Commands
  --

  -- ping/pong game
  | cmd == "PING" = do
  
    let trail = msgTrail msg
    sendMsg $ pongMsg trail

  -- join channels
  | cmd == "JOIN" = do

    let (trail:_)       = B8.words $ msgTrail msg
        channels        = B8.split ',' trail
        Just (Left who) = msgPrefix msg

    withUsr who $ \usr ->
      addMsg $ JoinMsg channels usr

  | cmd == "PART" = do

    let Just (Left who) = msgPrefix msg
        (chan:_) = msgParams msg

    -- send part message
    withUsr who $ \usr ->
      addMsg $ PartMsg chan usr

  | cmd == "QUIT" = do

    let Just (Left who) = msgPrefix msg
        comment = msgTrail msg

    withUsr who $ \usr ->
      addMsg $ QuitMsg usr (if BS.null comment then Nothing else Just comment)

  | cmd == "KICK" = do

    let (chan:who:_) = msgParams msg
        comment      = msgTrail msg

    withNick who $ \nick ->
      addMsg $ KickMsg chan nick
                       (if BS.null comment then Nothing else Just comment)

  | cmd == "KILL" = do

    quitCmd "KILL received (closing connection)"

  -- private messages
  | cmd == "PRIVMSG" = do

    let (to:_)    = msgParams msg
        Just from = msgPrefix msg
        txt       = msgTrail msg
    
    addMsg $ PrivMsg from to txt

  -- notice messages
  | cmd == "NOTICE" = do

    let (to:_)    = msgParams msg
        Just from = msgPrefix msg
        txt       = msgTrail msg
    
    addMsg $ NoticeMsg from to txt

  -- nick changes
  | cmd == "NICK" = do

    let (new:_)         = msgParams msg
        Just (Left who) = msgPrefix msg

    withUsr who $ \usr -> addMsg $ NickMsg usr new

  | cmd == "ERROR" = do

    let err = msgTrail msg
    quitCmd $ "ERROR: " ++ B8.unpack err

  --
  -- REPLIES
  --

  -- message of the day (MOTD)
  | cmd `isCode` rpl_MOTDSTART ||
    cmd `isCode` rpl_MOTD      = do

    addMsg $ MOTDMsg (msgTrail msg)

  -- end of MOTD
  | cmd `isCode` rpl_ENDOFMOTD = do

    doNothing

  -- topic reply
  | cmd `isCode` rpl_TOPIC = do

    let chan  = head $ msgParams msg
        topic = msgTrail msg

    addMsg $ TopicMsg chan (Just topic)

  -- remove old topic (if any)
  | cmd `isCode` rpl_NOTOPIC = do

    let chan = head $ msgParams msg

    addMsg $ TopicMsg chan Nothing

  -- channel names
  | cmd `isCode` rpl_NAMREPLY = do

    let (_:_:chan:_)   = msgParams msg
        namesWithFlags = map getUserflag $ B8.words $ msgTrail msg

    addMsg $ NamreplyMsg chan namesWithFlags

  -- end of channel names
  | cmd `isCode` rpl_ENDOFNAMES = do

    doNothing

  --
  -- ERROR codes
  --

  -- nick name change failure
  | cmd `isCode` err_NICKCOLLISION ||
    cmd `isCode` err_NICKNAMEINUSE = do

    addMsg $ ErrorMsg cmd

  --
  -- Other
  --

  | otherwise = do

    addMsg $ OtherMsg (msgPrefix msg)
                      (msgCmd msg)
                      (msgParams msg)
                      (msgTrail msg)

 where

  isCode bs code = bs == (B8.pack code)

  emptyRes, doNothing :: IncomingResult
  emptyRes  = IncomingResult [] Nothing Nothing
  doNothing = emptyRes

  quitCmd :: String -> IncomingResult
  quitCmd reason = emptyRes { quit = Just reason }

  addMsg :: Message -> IncomingResult
  addMsg msg' = emptyRes { add_msg = Just msg' }

  sendMsg :: IRCMsg -> IncomingResult
  sendMsg irc = emptyRes { send_msgs = [irc] }

  withUsr :: UserInfo -> (Maybe UserInfo -> IncomingResult) -> IncomingResult
  withUsr who f = IncomingReqUsr $ \usr ->
    if isCurrentUser usr who then f Nothing else f (Just who)

  withNick :: Nickname -> (Maybe Nickname -> IncomingResult) -> IncomingResult
  withNick who f = IncomingReqNck $ \cur_nick ->
    if who == cur_nick then f Nothing else f (Just who)
