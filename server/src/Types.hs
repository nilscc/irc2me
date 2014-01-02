module Types where

import Control.Concurrent.STM

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Time

import Network
import Network.IRC.ByteString.Parser

import System.IO

type Nickname = ByteString
type Username = ByteString
type Realname = ByteString

data User = User
  { usr_nick     :: Nickname
  , usr_nick_alt :: [Nickname] -- ^ alternative nicks (when nickname in use)
  , usr_name     :: Username
  , usr_realname :: Realname
  }

data Userflag = Operator | Voice
  deriving (Show, Eq, Ord)

data Server = Server
  { srv_host :: String
  , srv_port :: PortID
  }

type Channel = ByteString
type Key     = ByteString

data Message
  = PrivMsg { privmsg_from    :: Either UserInfo ServerName
            , privmsg_to      :: ByteString
            , privmsg_content :: ByteString }

  | NoticeMsg { noticemsg_from    :: Either UserInfo ServerName
              , noticemsg_to      :: ByteString
              , noticemsg_content :: ByteString }

  | JoinMsg { joinmsg_channel :: Channel
            , joinmsg_who     :: Maybe UserInfo }

  | PartMsg { partmsg_channel :: Channel
            , partmsg_who     :: Maybe UserInfo } -- Nothing if current user is
                                                  -- leaving channel
                                                  --
  | KickMsg { kickmsg_channel :: Channel
            , kickmsg_who     :: Maybe ByteString -- Nothing if current user is
                                                  -- being kicked
            , kickmsg_comment :: Maybe ByteString }

data Connection = Connection
  { con_user            :: User
  , con_nick_cur        :: Nickname
  , con_server          :: Server
  , con_channels        :: Map Channel (Maybe Key)
  , con_channelsettings :: Map Channel ChannelSettings
  , con_handle          :: Handle
  , con_debug_output    :: TChan String
  , con_messages        :: TChan (UTCTime, Message)
  }

data ChannelSettings = ChannelSettings
  { chan_topic  :: Maybe ByteString
  , chan_names  :: Set (Nickname, Maybe Userflag)
  }
