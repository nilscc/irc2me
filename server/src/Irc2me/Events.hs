{-# LANGUAGE DataKinds #-}

module Irc2me.Events where

import Control.Concurrent.Event

import Irc2me.Database.Tables.Accounts

data AccountEvent = AccountEvent { _eventAccountId :: AccountID, _event :: Event }
  deriving (Eq, Show)

data Event
  = ClientConnected
  deriving (Eq, Show)

type EventRW m a = EventT RW AccountEvent m a
type EventWO m a = EventT WO AccountEvent m a

--------------------------------------------------------------------------------
-- IRC events

