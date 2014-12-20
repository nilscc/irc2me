{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -fno-warn-orphans #-} -- for Show instance of SendServerMessage

module Irc2me.Events where

-- pipes
import Pipes

-- local
import Control.Concurrent.Event

import Irc2me.Frontend.Pipes
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Connection.Types
import Irc2me.Database.Tables.Accounts

data AccountEvent = AccountEvent { _eventAccountId :: AccountID, _event :: Event }
  deriving (Show)

data Event
  = ClientConnected SendServerMessage
  -- | SendMessage  NetworkID IrcMessage
  deriving (Show)

type SendServerMessage = (ServerMessage -> IO ())

instance Show (ServerMessage -> IO ()) where
  show _ = "(ServerMessage -> IO ())"

type EventRW m = EventT RW AccountEvent m
type EventWO m = EventT WO AccountEvent m

--------------------------------------------------------------------------------
-- IRC events

clientConnected
  :: ClientConnection c
  => AccountID
  -> c
  -> AccountEvent
clientConnected aid c = AccountEvent aid $ ClientConnected $ \serverMsg -> do

    -- send message
    runEffect $ yield serverMsg >-> encodeMsg >-> send

 where
  send = await >>= lift . sendToClient c

--sendMessage :: AccountID -> NetworkID -> ChatMessage -> AccountEvent
--sendMessage aid nid msg = AccountEvent aid $ SendMessage nid msg
