{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module ProtoBuf.Messages.Network where

import Data.ProtocolBuffers
import Data.Text (Text)
import Data.Int (Int32, Int64)
import Data.Monoid

import Control.Lens.TH

import GHC.Generics (Generic)

import ProtoBuf.Instances ()

data PB_Server = PB_Server
  { _server_host       :: Optional 1  (Value Text)
  , _server_port       :: Optional 2  (Value Int32)
  , _server_use_tls    :: Optional 10 (Value Bool)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Server
instance Decode PB_Server

makeLenses ''PB_Server

emptyServer
  :: Text     -- ^ Hostname
  -> Int      -- ^ Port number
  -> PB_Server
emptyServer hn p = PB_Server
  (putField $ Just hn)
  (putField $ Just $ fromIntegral p)
  mempty

data PB_Network = PB_Network
  { _network_id        :: Required 1  (Value Int64)

    -- network settings
  , _network_name      :: Optional 10 (Value Text)
  , _network_servers   :: Repeated 11 (Message PB_Server)
  , _network_reconnect :: Optional 12 (Value Bool)

    -- user commands
  , _network_commands  :: Repeated 20 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Network
instance Decode PB_Network

makeLenses ''PB_Network

emptyNetwork
  :: Integer        -- ^ Network ID
  -> PB_Network
emptyNetwork n_id = PB_Network
  (putField $ fromIntegral n_id)
  -- network settings
  mempty
  mempty
  mempty
  -- user commands
  mempty
