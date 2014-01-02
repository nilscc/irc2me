module Types where

import Control.Concurrent.STM

import Network
--import Network.IRC.ByteString.Parser

import System.IO

data User = User
  { usr_nick     :: String
  , usr_nick_alt :: [String] -- ^ alternative nicks (when nickname in use)
  , usr_name     :: String
  , usr_realname :: String
  }

data Server = Server
  { srv_host :: String
  , srv_port :: PortID
  }

data Connection = Connection
  { con_user         :: User
  , con_nick_cur     :: String
  , con_nick_alt     :: [String]
  , con_server       :: Server
  , con_handle       :: Handle
  , con_debug_output :: TChan String
  }
