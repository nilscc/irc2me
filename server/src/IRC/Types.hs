module IRC.Types where

import Control.Concurrent
import Control.Concurrent.STM

import Data.ByteString (ByteString)
import Data.Map (Map)

import           Network
import qualified Network.TLS                    as TLS
import           Network.IRC.ByteString.Parser

import System.IO

type Nickname = ByteString
type Username = ByteString
type Realname = ByteString

data Identity = Identity
  { usr_nick     :: Nickname
  , usr_nick_alt :: [Nickname] -- ^ alternative nicks (when nickname in use)
  , usr_name     :: Username
  , usr_realname :: Realname
  }
  deriving (Show, Eq, Ord)

data Userflag = Operator | Voice
  deriving (Show, Eq, Ord, Enum)

data Server = Server
  { srv_host      :: String
  , srv_port      :: PortID
  , srv_tls       :: TLSSettings
  , srv_reconnect :: Bool
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

  | JoinMsg { joinmsg_channels :: [Channel]
            , joinmsg_who      :: Maybe UserInfo }

  | PartMsg { partmsg_channel :: Channel
            , partmsg_who     :: Maybe UserInfo } -- Nothing if current user is
                                                  -- leaving channel

  | QuitMsg { quitmsg_who      :: Maybe UserInfo   -- Nothing if current user is
                                                   -- leaving channel
            , quitmsg_comment  :: Maybe ByteString
            }

  | KickMsg { kickmsg_channel :: Channel
            , kickmsg_who     :: Maybe ByteString -- Nothing if current user is
                                                  -- being kicked
            , kickmsg_comment :: Maybe ByteString }

  | MOTDMsg { motd_content :: ByteString }

  | NickMsg { nickmsg_old :: Maybe UserInfo
            , nickmsg_new :: Nickname }

  | ErrorMsg { errormsg_code :: ByteString }

  | TopicMsg { topicmsg_channel :: Channel
             , topicmsg_topic   :: Maybe ByteString }

  | NamreplyMsg { namreply_channel :: Channel
                , namreply_names   :: [(Nickname, Maybe Userflag)]
                }

  | OtherMsg { othermsg_from    :: Maybe (Either UserInfo ServerName)
             , othermsg_cmd     :: ByteString
             , othermsg_params  :: [ByteString]
             , othermsg_content :: ByteString }

type TLSBuffer = TVar ByteString

data ConnectionStatus
  = ConnectionInitializing
  | ConnectionEstablished
  | ConnectionClosed
  deriving (Eq, Show)

data Connection = Connection
  { -- read only settings:
    con_user            :: Identity
  , con_server          :: Server
    -- connection state variables:
  , con_nick_cur        :: TVar Nickname
  , con_channels        :: TVar (Map Channel (Maybe Key))
  , con_handle          :: Handle
  , con_status          :: TVar ConnectionStatus
  , con_tls_context     :: TVar (Maybe (TLS.Context, TLSBuffer, ThreadId))
  , con_debug_output    :: TChan String
  }

data ChannelSettings = ChannelSettings
  { chan_topic  :: Maybe ByteString
  , chan_names  :: Map Nickname (Maybe Userflag)
  }

data TLSSettings
  = NoTLS                     -- ^ No TLS, plaintext only
  | TLS                       -- ^ Start with TLS handshake
  | STARTTLS                  -- ^ Start plaintext and send STARTTLS. Close
                              -- connection if TLS is not available.
  | OptionalSTARTTLS          -- ^ Try to use STARTTLS, use plaintext if not
                              -- available
  deriving (Eq, Show)
