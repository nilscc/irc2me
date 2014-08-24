{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module ProtoBuf.Messages.Network where

import Data.ProtocolBuffers
import Data.Text (Text)
import Data.Int (Int32)
import Data.Monoid

import Control.Lens.Operators
import Control.Lens.TH

import GHC.Generics (Generic)

import ProtoBuf.Types
import ProtoBuf.Helper
import qualified IRC.Types as IRC
import ProtoBuf.Instances ()

import ProtoBuf.Messages.Channel

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
  { _network_id        :: Optional 1  (Value ID_T)

    -- status
  , _network_online    :: Optional 5  (Value Bool)

    -- network settings
  , _network_name      :: Optional 10 (Value Text)
  , _network_reconnect :: Optional 11 (Value Bool)
  , _network_identity  :: Optional 12 (Value ID_T)

    -- network servers
  , _network_servers   :: Repeated 20 (Message PB_Server)

    -- network channels
  , _network_channels  :: Repeated 30 (Message PB_Channel)

    -- user commands
  -- , _network_commands  :: Repeated 20 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Network
instance Decode PB_Network

makeLenses ''PB_Network

emptyNetwork
  :: Integer        -- ^ Network ID
  -> PB_Network
emptyNetwork n_id = PB_Network
  (putField $ Just $ fromIntegral n_id)
  -- network status
  mempty
  -- network settings
  mempty
  mempty
  mempty
  -- network servers
  mempty
  -- network channels
  mempty
  -- user commands
  --mempty

encodeNetwork :: IRC.Network -> PB_Network
encodeNetwork netw = emptyNetwork (IRC.netw_id netw)
  & network_name      .~~ Just (IRC.netw_name netw)
  & network_reconnect .~~ Just (IRC.netw_reconnect netw)
  & network_identity  .~~ IRC.netw_identity netw
