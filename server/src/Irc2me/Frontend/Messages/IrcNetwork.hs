{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Irc2me.Frontend.Messages.IrcNetwork where

import Data.Text (Text)
import Data.Int

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH

--local
import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages.IrcMessage

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

emptyIrcServer :: IrcServer
emptyIrcServer = IrcServer
  { _serverHost = putField Nothing
  , _serverPort = putField Nothing
  , _serverUseTLS = putField Nothing
  }

--
-- IRC network message
--

data IrcNetwork = IrcNetwork
  { _networkId        :: Optional 1  (Value ID_T)

    -- status
  , _networkOnline    :: Optional 5  (Value Bool)

    -- network settings
  , _networkName      :: Optional 10 (Value Text)
  , _networkReconnect :: Optional 11 (Value Bool)
  , _networkIdentity  :: Optional 12 (Value ID_T)

    -- network servers
  -- , _networkServers   :: Repeated 20 (Message IrcServer)

    -- network channels
  -- , _network_channels  :: Repeated 30 (Message IrcChannel)

  , _networkMessages  :: Repeated 40 (Message IrcMessage)
  }
  deriving (Eq, Show, Generic)

instance Encode IrcNetwork
instance Decode IrcNetwork

emptyIrcNetwork :: IrcNetwork
emptyIrcNetwork = IrcNetwork
  { _networkId        = putField Nothing
  , _networkName      = putField Nothing
  , _networkOnline    = putField Nothing
  , _networkReconnect = putField Nothing
  , _networkIdentity  = putField Nothing
  , _networkMessages  = putField []
  }

------------------------------------------------------------------------------
-- Lanes

makeFieldLenses ''IrcServer
makeFieldLenses ''IrcNetwork
