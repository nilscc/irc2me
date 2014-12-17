{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -fno-warn-orphans #-} -- for Show instance of SendServerMessage

module Irc2me.Events.Types where

-- local
import Control.Concurrent.Event

import Irc2me.Backends.IRC.Helper
import Irc2me.Frontend.Messages
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks

data AccountEvent = AccountEvent { _eventAccountId :: AccountID, _event :: Event }
  deriving (Show)

data Event
  = ClientConnectedEvent                (ServerMessage -> IO ())
  | ClientMessageEvent    ClientMessage (ServerMessage -> IO ())
  | NewIrcConnectionEvent NetworkID IrcConnection
  | ChatMessageEvent      NetworkID ChatMessage
  deriving (Show)

instance Show (ServerMessage -> IO ()) where
  show _ = "(ServerMessage -> IO ())"

type EventRW m = EventT RW AccountEvent m
type EventWO m = EventT WO AccountEvent m
