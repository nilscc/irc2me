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

data PrivateQuery = PrivateQuery
  { _queryUser        :: Required 1  (Message User)
  , _queryStatus      :: Optional 2  (Enumeration ChannelStatus)
  , _queryMessages    :: Repeated 10 (Message ChatMessage)
  }
  deriving (Eq, Show, Generic)

instance Encode PrivateQuery
instance Decode PrivateQuery

emptyPrivateQuery :: User -> PrivateQuery
emptyPrivateQuery u = PrivateQuery
  { _queryUser      = putField u
  , _queryStatus    = putField Nothing
  , _queryMessages  = putField []
  }

data Channel = Channel
  { _channelId        :: Optional 1 (Value ID_T)
  , _channelName      :: Optional 2 (Value Text)
  , _channelStatus    :: Optional 3 (Enumeration ChannelStatus)
  , _channelMessages  :: Repeated 10 (Message ChatMessage)
  , _channelUsers     :: Repeated 20 (Message User)
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
  , _channelUsers = putField []
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

    -- network messages
  , _networkMessages  :: Repeated 30 (Message ChatMessage)

    -- private query messages
  , _networkQueries   :: Repeated 40 (Message PrivateQuery)

    -- channels
  , _networkChannels  :: Repeated 50 (Message Channel)
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
  , _networkMessages  = putField []
  , _networkQueries   = putField []
  , _networkChannels  = putField []
  }

------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''Server
makeFieldLenses ''PrivateQuery
makeFieldLenses ''Channel
makeFieldLenses ''Network
