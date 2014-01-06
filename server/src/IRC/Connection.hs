{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Module with helper functions for "Connection" state modifications
module IRC.Connection where

import Control.Concurrent.STM
import Control.Monad

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Time

import Network.IRC.ByteString.Parser

import IRC.Types

--------------------------------------------------------------------------------
-- Connection

waitForInitialization :: Connection -> IO ()
waitForInitialization con = atomically $ do
  stat <- getConnectionStatus' con
  when (stat == ConnectionInitializing) retry

getConnectionStatus :: Connection -> IO ConnectionStatus
getConnectionStatus = atomically . getConnectionStatus'

getConnectionStatus' :: Connection -> STM ConnectionStatus
getConnectionStatus' con = readTVar (con_status con)

-- | Check is connection is open. Blocks if connection is still initializing
-- until init is done.
isOpenConnection :: Connection -> IO Bool
isOpenConnection con = atomically $ isOpenConnection' con

isOpenConnection' :: Connection -> STM Bool
isOpenConnection' con =
  (ConnectionEstablished ==) `fmap` getConnectionStatus' con

isInitConnection :: Connection -> IO Bool
isInitConnection con = atomically $ isInitConnection' con

isInitConnection' :: Connection -> STM Bool
isInitConnection' con =
  (ConnectionInitializing ==) `fmap` getConnectionStatus' con

setConnectionStatus :: Connection -> ConnectionStatus -> IO ()
setConnectionStatus con stat = atomically $ setConnectionStatus' con stat

setConnectionStatus' :: Connection -> ConnectionStatus -> STM ()
setConnectionStatus' con stat = writeTVar (con_status con) stat

--------------------------------------------------------------------------------
-- Nick

setNick :: Connection -> ByteString -> IO ()
setNick con nick = atomically $ setNick' con nick

setNick' :: Connection -> ByteString -> STM ()
setNick' con nick = writeTVar (con_nick_cur con) nick

--------------------------------------------------------------------------------
-- Channels

getChannels :: Connection -> IO (Map Channel (Maybe Key))
getChannels con = atomically $ getChannels' con

getChannels' :: Connection -> STM (Map Channel (Maybe Key))
getChannels' con = readTVar (con_channels con)

changeChannelSettings
  :: Connection
  -> (Map Channel ChannelSettings -> Map Channel ChannelSettings)
  -> IO ()
changeChannelSettings con f = atomically $ changeChannelSettings' con f

changeChannelSettings'
  :: Connection
  -> (Map Channel ChannelSettings -> Map Channel ChannelSettings)
  -> STM ()
changeChannelSettings' con f = modifyTVar (con_channelsettings con) f

adjustChannelSettings
  :: Connection -> Channel -> (ChannelSettings -> ChannelSettings) -> IO ()
adjustChannelSettings con channel f =
  changeChannelSettings con (M.adjust f channel)

-- | Add new message with current time
addMessage :: Connection -> UTCTime -> Message -> IO ()
addMessage con' time msg = do

  atomically $ writeTChan (con_messages con') (time, msg)

-- | Add a list of channels with new (empty) ChannelSettings to current
-- connection
addChannels :: Connection -> [Channel] -> IO ()
addChannels con channels =
  forM_ channels $ \chan ->
    changeChannelSettings con $ M.insert chan (ChannelSettings Nothing M.empty)

-- | Set topic for a given channel
setTopic :: Connection -> Channel -> Maybe ByteString -> IO ()
setTopic con channel mtopic =
  adjustChannelSettings con channel $ \s -> s { chan_topic = mtopic }

-- | Split "[@|+]<nick>"
getUserflag :: ByteString -> (Nickname, Maybe Userflag)
getUserflag n = case B8.uncons n of
                  Just ('@', nick) -> (nick, Just Operator)
                  Just ('+', nick) -> (nick, Just Voice)
                  _                -> (n, Nothing)

-- | Set channel nicknames
setChanNames
  :: Connection -> Channel -> [(Nickname, Maybe Userflag)] -> IO ()
setChanNames con' channel namesWithFlags =
  adjustChannelSettings con' channel $ \s ->
    s { chan_names = M.fromList namesWithFlags }

-- | Add a user to a list of channels
addUser :: Connection -> UserInfo -> [Channel] -> IO ()
addUser con who channels =
  forM_ channels $ \chan ->
    adjustChannelSettings con chan $ \s ->
      s { chan_names = M.insert (userNick who) Nothing
                                (chan_names s) }

-- | Compare a IRC message prefix with current nickname & username
isCurrentUser :: Connection -> UserInfo -> IO Bool
isCurrentUser con who = atomically $ isCurrentUser' con who

isCurrentUser' :: Connection -> UserInfo -> STM Bool
isCurrentUser' con (UserInfo{ userNick, userName }) = do
  cur <- readTVar $ con_nick_cur con
  return $ userNick == cur &&
           userName == Just (usr_name (con_user con))

-- | Compare nicknames only
isCurrentNick :: Connection -> Nickname -> IO Bool
isCurrentNick con nick = atomically $ isCurrentNick' con nick

isCurrentNick' :: Connection -> Nickname -> STM Bool
isCurrentNick' con nick = (nick ==) `fmap` readTVar (con_nick_cur con)

-- | Leave and remove a channel from current connection
leaveChannel :: Connection -> Channel -> IO ()
leaveChannel con channel = atomically $ do
  modifyTVar (con_channels con)        $ M.delete channel
  modifyTVar (con_channelsettings con) $ M.delete channel

-- | Remove user from channel settings
removeUser :: Connection -> Channel -> Nickname -> IO ()
removeUser con' channel nick =
  adjustChannelSettings con' channel $ \s ->
    s { chan_names = M.delete nick (chan_names s) }

-- | Remove user from all channels
userQuit :: Connection -> [Channel] -> Nickname -> IO ()
userQuit con chans nick =
  forM_ chans $ \chan -> removeUser con chan nick

-- | Change all occurrences of a given nickname
changeNickname :: Connection -> UserInfo -> Nickname -> IO ()
changeNickname con who@UserInfo{ userNick = old } new = atomically $ do

  changeChannelSettings' con $ M.map $ \s ->
    let names = chan_names s
        flag  = join $ M.lookup old names
     in if old `M.member` names
          then s { chan_names = M.insert new flag $
                                  M.delete old names }
          else s

  -- also check whether we have to change our own nickname
  is_cur <- isCurrentUser' con who
  when is_cur $
    writeTVar (con_nick_cur con) new

-- | Get all channels with a certain user
getChannelsWithUser :: Connection -> Nickname -> IO [Channel]
getChannelsWithUser con nick = atomically $ getChannelsWithUser' con nick

-- | Get all channels with a certain user
getChannelsWithUser' :: Connection -> Nickname -> STM [Channel]
getChannelsWithUser' con nick = do
  chans <- readTVar $ con_channelsettings con
  return $ M.keys $ M.filter (\s -> nick `M.member` chan_names s) chans
