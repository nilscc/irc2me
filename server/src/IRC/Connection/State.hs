{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Module with helper functions for "Connection" state modifications
module IRC.Connection.State where

import Control.Concurrent.STM
import Control.Monad
import Control.Lens.Operators

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

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
getConnectionStatus' con = readTVar (con ^. con_status)

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
setConnectionStatus' con stat = writeTVar (con ^. con_status) stat

--------------------------------------------------------------------------------
-- Nick

setNick :: Connection -> ByteString -> IO ()
setNick con nick = atomically $ setNick' con nick

setNick' :: Connection -> ByteString -> STM ()
setNick' con nick = writeTVar (con ^. con_nick_cur) nick

--------------------------------------------------------------------------------
-- Channels

getChannels :: Connection -> IO (Map Channel (Maybe Key))
getChannels con = atomically $ getChannels' con

getChannels' :: Connection -> STM (Map Channel (Maybe Key))
getChannels' con = readTVar (con ^. con_channels)

setChannels :: Connection -> [(Channel, Maybe Key)] -> IO ()
setChannels con chans = atomically $ setChannels' con chans

setChannels' :: Connection -> [(Channel, Maybe Key)] -> STM ()
setChannels' con chans = writeTVar (con ^. con_channels) (M.fromList chans)

-- | Split "[@|+]<nick>"
getUserflag :: ByteString -> (Nickname, Maybe Userflag)
getUserflag n = case B8.uncons n of
                  Just ('@', nick) -> (nick, Just Operator)
                  Just ('+', nick) -> (nick, Just Voice)
                  _                -> (n, Nothing)

getCurrentNickname :: Connection -> STM Nickname
getCurrentNickname con = readTVar $ con ^. con_nick_cur

-- | Compare a IRC message prefix with current nickname & username
isCurrentUser :: Identity -> UserInfo -> Bool
isCurrentUser usr (UserInfo{ userNick, userName }) =
  userNick == usr ^. ident_nick &&
  userName == Just (usr ^. ident_name)

-- | Compare nicknames only
isCurrentNick :: Connection -> Nickname -> IO Bool
isCurrentNick con nick = atomically $ isCurrentNick' con nick

isCurrentNick' :: Connection -> Nickname -> STM Bool
isCurrentNick' con nick = (nick ==) `fmap` readTVar (con ^. con_nick_cur)
