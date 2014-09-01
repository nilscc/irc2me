{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Irc2me.ProtoBuf.Messages.Network where

import Data.Text (Text)
import Data.Int
import Data.Monoid

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH

type ID_T = Int64

--
-- IRC server
--

data IrcServer = IrcServer
  { _serverHost       :: Optional 1  (Value Text)
  , _serverPort       :: Optional 2  (Value Int32)
  , _serverUseTLS     :: Optional 10 (Value Bool)
  }
  deriving (Eq, Show, Generic)

instance Encode IrcServer
instance Decode IrcServer

emptyServer
  :: Text     -- ^ Hostname
  -> Int      -- ^ Port number
  -> IrcServer
emptyServer hn p = IrcServer
  (putField $ Just hn)
  (putField $ Just $ fromIntegral p)
  mempty

--
-- IRC network message
--

data IrcNetwork = IrcNetwork
  { _networkID        :: Optional 1  (Value ID_T)

    -- status
  -- , _networkOnline    :: Optional 5  (Value Bool)

    -- network settings
  -- , _networkName      :: Optional 10 (Value Text)
  -- , _networkReconnect :: Optional 11 (Value Bool)
  -- , _networkIdentity  :: Optional 12 (Value ID_T)

    -- network servers
  -- , _networkServers   :: Repeated 20 (Message IrcServer)

    -- network channels
  -- , _network_channels  :: Repeated 30 (Message IrcChannel)

  }
  deriving (Eq, Show, Generic)

instance Encode IrcNetwork
instance Decode IrcNetwork

emptyIrcNetwork :: IrcNetwork
emptyIrcNetwork = IrcNetwork
  { _networkID = putField Nothing
  }

------------------------------------------------------------------------------
-- Lanes

makeFieldLenses ''IrcServer
makeFieldLenses ''IrcNetwork
