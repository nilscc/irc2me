{-# LANGUAGE TemplateHaskell #-}

module Irc2me.Backends.IRC.Types where

import Control.Concurrent
import Control.Concurrent.STM.TChan (TChan)
import Control.Lens hiding (Identity)

import Data.Time (UTCTime)
import Data.Map (Map)
import Data.Text (Text)

import qualified Irc2me.Types as Ty
import Network.IRC.ByteString.Parser (IRCMsg)

--------------------------------------------------------------------------------
-- IRC connection

data IrcConnection = IrcConnection
  { _ircThread        :: ThreadId
  , _ircSend          :: IRCMsg -> IO ()
  , _ircMessages      :: TChan (UTCTime, IRCMsg)
  }

makeLenses ''IrcConnection

type NetworkID = Ty.ID
type IdentityID = Ty.ID

type IrcConnections = Map Ty.AccountID (Map NetworkID IrcConnection)

--------------------------------------------------------------------------------
-- Users & identities

data Identity = Identity
  { _identityId        :: IdentityID
  , _identityNick      :: Text
  -- , identityAlt   :: [Text]
  , _identityName      :: Text
  , _identityRealname  :: Text
  }
  deriving (Show, Eq, Ord)

makeLenses ''Identity

--------------------------------------------------------------------------------
-- Servers & networks

data Server = Server
  { _serverHost   :: String
  , _serverPort   :: Int
  , _serverUseTLS :: Bool
  }
  deriving (Show, Eq, Ord)

makeLenses ''Server

data Network = Network
  { _networkId        :: Integer
  , _networkName      :: Text
  -- , _networkOnline    :: Bool
  , _networkReconnect :: Bool
  , _networkIdentity  :: Maybe IdentityID
  , _networkServers   :: [Server]
  }
  deriving (Show, Eq, Ord)

makeLenses ''Network

--------------------------------------------------------------------------------
-- Channels & messages

data Channel = Channel
  {
  }
  deriving (Show, Eq, Ord)

makeLenses ''Channel
