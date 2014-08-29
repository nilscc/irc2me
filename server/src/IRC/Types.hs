{-# LANGUAGE TemplateHaskell #-}

module IRC.Types where

import Control.Concurrent
import Control.Concurrent.STM

import Data.ByteString (ByteString)
import Data.Map (Map)

import Control.Lens.TH

import           Network
import qualified Network.TLS                    as TLS
import           Network.IRC.ByteString.Parser

import System.IO

type ID = Integer

type Nickname = ByteString
type Username = ByteString
type Realname = ByteString

data Identity = Identity
  { _ident_id       :: ID
  , _ident_nick     :: Nickname
  , _ident_nick_alt :: [Nickname] -- ^ alternative nicks (when nickname in use)
  , _ident_name     :: Username
  , _ident_realname :: Realname
  }
  deriving (Show, Eq, Ord)

data Userflag = Operator | Voice
  deriving (Show, Eq, Ord, Enum)

data Network = Network
  { _netw_id         :: ID
  , _netw_name       :: String
  , _netw_reconnect  :: Bool
  , _netw_identity   :: Maybe ID
  }
  deriving (Show, Eq)

data Server = Server
  { _srv_host      :: String
  , _srv_port      :: PortID
  , _srv_tls       :: TLSSettings
  }
  deriving (Show, Eq)

type Channel = ByteString
type Key     = ByteString

data Message
  = PrivMsg     { _privmsg_from    :: Either UserInfo ServerName
                , _privmsg_to      :: ByteString
                , _privmsg_content :: ByteString }

  | NoticeMsg   { _noticemsg_from    :: Either UserInfo ServerName
                , _noticemsg_to      :: ByteString
                , _noticemsg_content :: ByteString }

  | JoinMsg     { _joinmsg_channels :: [Channel]
                , _joinmsg_who      :: Maybe UserInfo }

  | PartMsg     { _partmsg_channel :: Channel
                , _partmsg_who     :: Maybe UserInfo } -- Nothing if current user is
                                                       -- leaving channel

  | QuitMsg     { _quitmsg_who      :: Maybe UserInfo   -- Nothing if current user is
                                                        -- leaving channel
                , _quitmsg_comment  :: Maybe ByteString
                }

  | KickMsg     { _kickmsg_channel :: Channel
                , _kickmsg_who     :: Maybe ByteString -- Nothing if current user is
                                                       -- being kicked
                , _kickmsg_comment :: Maybe ByteString }

  | MOTDMsg     { _motd_content :: ByteString }

  | NickMsg     { _nickmsg_old :: Maybe UserInfo
                , _nickmsg_new :: Nickname }

  | ErrorMsg    { _errormsg_code :: ByteString }

  | TopicMsg    { _topicmsg_channel :: Channel
                , _topicmsg_topic   :: Maybe ByteString }

  | NamreplyMsg { _namreply_channel :: Channel
                , _namreply_names   :: [(Nickname, Maybe Userflag)]
                }

  | OtherMsg    { _othermsg_from    :: Maybe (Either UserInfo ServerName)
                , _othermsg_cmd     :: ByteString
                , _othermsg_params  :: [ByteString]
                , _othermsg_content :: ByteString }

type TLSBuffer = TVar ByteString

data ConnectionStatus
  = ConnectionInitializing
  | ConnectionEstablished
  | ConnectionClosed
  deriving (Eq, Show)

data Connection = Connection
  { -- read only settings:
    _con_user            :: Identity
  , _con_server          :: Server
    -- connection state variables:
  , _con_nick_cur        :: TVar Nickname
  , _con_channels        :: TVar (Map Channel (Maybe Key))
  , _con_handle          :: Handle
  , _con_status          :: TVar ConnectionStatus
  , _con_tls_context     :: TVar (Maybe (TLS.Context, TLSBuffer, ThreadId))
  , _con_debug_output    :: TChan String
  }

data ChannelSettings = ChannelSettings
  { _chan_topic  :: Maybe ByteString
  , _chan_names  :: Map Nickname (Maybe Userflag)
  }

data TLSSettings
  = NoTLS                     -- ^ No TLS, plaintext only
  | TLS                       -- ^ Start with TLS handshake
  | STARTTLS                  -- ^ Start plaintext and send STARTTLS. Close
                              -- connection if TLS is not available.
  | OptionalSTARTTLS          -- ^ Try to use STARTTLS, use plaintext if not
                              -- available
  deriving (Eq, Show)

--
-- template haskell
--

makeLenses ''Identity
makePrisms ''Userflag
makeLenses ''Network
makeLenses ''Server
makeLenses ''Message
makePrisms ''ConnectionStatus
makeLenses ''Connection
makeLenses ''ChannelSettings
makePrisms ''TLSSettings
