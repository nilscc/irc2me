module Types where

import Control.Concurrent.STM

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time

import Network
--import Network.IRC.ByteString.Parser

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
  deriving Show
  
data Server = Server
  { srv_host :: String
  , srv_port :: PortID
  }

type Channel = ByteString
type Key     = ByteString

data Message
  = PrivMsg { privmsg_to      :: ByteString
            , privmsg_content :: ByteString }

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
  , chan_names  :: [(Nickname, Maybe Userflag)]
  }
