{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Irc2me.Frontend.Messages.Network where

import Data.Text (Text)
import Data.Int

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH

--local
import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages.Identity
import Irc2me.Frontend.Messages.ChatMessage

--
-- IRC server
--

data Server = Server
  { _serverHost       :: Optional 1  (Value Text)
  , _serverPort       :: Optional 2  (Value Int32)
  , _serverUseTLS     :: Optional 10 (Value Bool)
  }
  deriving (Eq, Show, Generic)

instance Encode Server
instance Decode Server

emptyServer :: Server
emptyServer = Server
  { _serverHost = putField Nothing
  , _serverPort = putField Nothing
  , _serverUseTLS = putField Nothing
  }

--
-- IRC channels
--

data ChannelStatus
  = OFFLINE
  | ONLINE
  | NEWMESSAGES
  | NEWNOTIFICATIONS
  deriving (Eq, Show, Enum)

data Channel = Channel
  { _channelId        :: Optional 1 (Value ID_T)
  , _channelName      :: Optional 2 (Value Text)
  , _channelStatus    :: Optional 3 (Enumeration ChannelStatus)
  , _channelMessages  :: Repeated 10 (Message ChatMessage)
  }
  deriving (Eq, Show, Generic)

instance Encode Channel
instance Decode Channel

emptyChannel :: Channel
emptyChannel = Channel
  { _channelId = putField Nothing
  , _channelName = putField Nothing
  , _channelStatus = putField Nothing
  , _channelMessages = putField []
  }


--
-- IRC network message
--

data Network = Network
  { _networkId        :: Optional 1  (Value ID_T)

    -- status
  , _networkOnline    :: Optional 5  (Value Bool)

    -- network settings
  , _networkName      :: Optional 10 (Value Text)
  , _networkReconnect :: Optional 11 (Value Bool)
  , _networkIdentity  :: Optional 12 (Message Identity)

    -- network servers
  , _networkServers   :: Repeated 20 (Message Server)

    -- channels
  , _networkChannels  :: Repeated 30 (Message Channel)

    -- network messages
  , _networkMessages  :: Repeated 40 (Message ChatMessage)
  }
  deriving (Eq, Show, Generic)

instance Encode Network
instance Decode Network

emptyNetwork :: Network
emptyNetwork = Network
  { _networkId        = putField Nothing
  , _networkName      = putField Nothing
  , _networkOnline    = putField Nothing
  , _networkReconnect = putField Nothing
  , _networkIdentity  = putField Nothing
  , _networkServers   = putField []
  , _networkChannels  = putField []
  , _networkMessages  = putField []
  }

------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''Server
makeFieldLenses ''Channel
makeFieldLenses ''Network
