{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module ProtoBuf.Network where

import Data.ProtocolBuffers
import Data.Text (Text)
import Data.Int (Int32)

import GHC.Generics (Generic)

import ProtoBuf.Instances ()

data PB_Network = PB_Network
  { network_name      :: Required 1  (Value Text)
  , network_servers   :: Repeated 2  (Message PB_Server)
    -- network settings
  , network_reconnect :: Optional 11 (Value Bool)
    -- user commands
  , network_commands  :: Repeated 20 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Network
instance Decode PB_Network

data PB_Server = PB_Server
  { server_host       :: Required 1  (Value Text)
  , server_port       :: Required 2  (Value Int32)
  , server_use_tls    :: Optional 10 (Value Bool)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Server
instance Decode PB_Server
