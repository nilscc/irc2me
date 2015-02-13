{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS -fno-warn-orphans #-} -- for Show instance of SendServerMessage

module Irc2me.Events.Types where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

-- text
import Data.Text (Text)

-- lens
import Control.Lens hiding (Identity)

-- local
import Control.Concurrent.Event

import Irc2me.Backends.IRC.Helper
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Connection.Types
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks

------------------------------------------------------------------------------
-- Event types
--

-- data AccountEvent = AccountEvent { _eventAccountId :: AccountID, _event :: Event }
  -- deriving (Show)

data Event
  = ClientEvent               ClientID ClientEvent
  | AccountEvent              AccountID AccountEvent
  deriving (Show)

data ClientEvent
  = ClientConnectedEvent      ClientConnection
  | ClientAuthenticatedEvent  AccountID
  | ClientDisconnectedEvent
  deriving (Show)

data AccountEvent
  = ClientMessageEvent        ClientConnection ClientMessage
  | NewIrcConnectionEvent     NetworkID IrcConnection Identity
  | ChatMessageEvent          NetworkID (Maybe User) ChatMessage
  deriving (Show)

type EventRW m = EventT RW Event m
type EventWO m = EventT WO Event m

-- orphan Show instances

instance Show (ServerMessage -> IO ()) where
  show _ = "(ServerMessage -> IO ())"

deriving instance Show ClientConnection

------------------------------------------------------------------------------
-- Event loop state type
--

data EventLoopState = EventLoopState
  { _elsAccounts :: Map AccountID AccountState
  , _elsClients  :: Map ClientID  ClientState
  }

--
-- Account state
--

data AccountState = AccountState
  { _connectedIrcNetworks   :: Map NetworkID IrcState
  , _connectedClients       :: Set ClientID
  }

instance AsEmpty AccountState where
  _Empty = nearly (AccountState M.empty S.empty) $ \as ->
    S.null (_connectedClients     as) &&
    M.null (_connectedIrcNetworks as)

type Channelname = Text
type Nickname = Text

data IrcState = IrcState
  { _ircConnection :: IrcConnection
  , _ircIdentity   :: Identity
  , _ircChannels   :: Set Text
  , _ircUsers      :: Map Channelname (Set Nickname)
  }

--
-- Client state
--

newtype ClientID = ClientID Integer
  deriving (Eq, Show, Ord, Enum, Num)

data ClientState = ClientState
  { _clientConnection :: ClientConnection
  , _clientAccount    :: Maybe AccountID
  }

--
-- TH
--

makeLenses ''EventLoopState
makeLenses ''AccountState
makeLenses ''IrcState
makeLenses ''ClientState
