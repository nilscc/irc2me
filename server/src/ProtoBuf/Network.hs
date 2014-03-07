{-# LANGUAGE DeriveGeneric #-}

module ProtoBuf.Network where

import Data.ProtocolBuffers
import Data.TypeLevel.Num
import Data.Text (Text)
import Data.Int (Int32)

import GHC.Generics (Generic)

import ProtoBuf.Instances ()

data PB_Network = PB_Network
  { network_name      :: Required D1  (Value Text)
  , network_servers   :: Repeated D2  (Message PB_Server)
    -- network settings
  , network_reconnect :: Optional D11 (Value Bool)
    -- user commands
  , network_commands  :: Repeated D20 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Network
instance Decode PB_Network

data PB_Server = PB_Server
  { server_host       :: Required D1  (Value Text)
  , server_port       :: Required D2  (Value Int32)
  , server_use_tls    :: Optional D10 (Value Bool)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Server
instance Decode PB_Server
