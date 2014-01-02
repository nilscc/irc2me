{-# LANGUAGE NamedFieldPuns #-}

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

changeChannelSettings
  :: Connection
  -> (Map Channel ChannelSettings -> Map Channel ChannelSettings)
  -> Connection
changeChannelSettings con' f =
  con' { con_channelsettings = f (con_channelsettings con') }

adjustChannelSettings
  :: Connection -> Channel -> (ChannelSettings -> ChannelSettings) -> Connection
adjustChannelSettings con' channel f =
  changeChannelSettings con' (M.adjust f channel)

-- | Add new message with current time
addMessage :: Connection -> Message -> IO ()
addMessage con' msg = do

  now <- getCurrentTime
  atomically $ writeTChan (con_messages con') (now, msg)

-- | Add a list of channels with new (empty) ChannelSettings to current
-- connection
addChannels :: Connection -> [Channel] -> Connection
addChannels con' channels =
  foldr (\chan con'' -> changeChannelSettings con'' $
                          M.insert chan (ChannelSettings Nothing M.empty))
        con'
        channels

-- | Set topic for a given channel
setTopic :: Connection -> Channel -> Maybe ByteString -> Connection
setTopic con' channel mtopic =
  adjustChannelSettings con' channel $ \s -> s { chan_topic = mtopic }

-- | Split "[@|+]<nick>"
getUserflag :: ByteString -> (Nickname, Maybe Userflag)
getUserflag n = case B8.uncons n of
                  Just ('@', nick) -> (nick, Just Operator)
                  Just ('+', nick) -> (nick, Just Voice)
                  _                -> (n, Nothing)

-- | Set channel nicknames
setChanNames
  :: Connection -> Channel -> [(Nickname, Maybe Userflag)] -> Connection
setChanNames con' channel namesWithFlags =
  adjustChannelSettings con' channel $ \s ->
    s { chan_names = M.fromList namesWithFlags }

-- | Add a user to a list of channels
addUser :: Connection -> UserInfo -> [Channel] -> Connection
addUser con' who channels =
  foldr (\chan con'' -> adjustChannelSettings con'' chan $ \s ->
                          s { chan_names = M.insert (userNick who) Nothing
                                                    (chan_names s) })
        con'
        channels

-- | Compare a IRC message prefix with current nickname & username
isCurrentUser :: Connection -> UserInfo -> Bool
isCurrentUser con' (UserInfo{ userNick, userName }) =
  userNick == con_nick_cur con' &&
  userName == Just (usr_name (con_user con'))

-- | Compare nicknames only
isCurrentNick :: Connection -> Nickname -> Bool
isCurrentNick con' nick = con_nick_cur con' == nick

-- | Leave and remove a channel from current connection
leaveChannel :: Connection -> Channel -> Connection
leaveChannel con' channel =
  con' { con_channels        = M.delete channel (con_channels con')
       , con_channelsettings = M.delete channel (con_channelsettings con')
       }

-- | Remove user from channel settings
removeUser :: Connection -> Channel -> Nickname -> Connection
removeUser con' channel nick =
  adjustChannelSettings con' channel $ \s ->
    s { chan_names = M.delete nick (chan_names s) }

-- | Change all occurrences of a given nickname
changeNickname :: Connection -> UserInfo -> Nickname -> Connection
changeNickname con' who@UserInfo{ userNick = old } new =

  let con'' = changeChannelSettings con' $ M.map $ \s ->
        let names = chan_names s
            flag  = join $ M.lookup old names
         in if old `M.member` names
              then s { chan_names = M.insert new flag $
                                    M.delete old names }
              else s

   in -- also check whether we have to change our own nickname
      if isCurrentUser con' who
        then con'' { con_nick_cur = new }
        else con''
