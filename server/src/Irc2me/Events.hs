{-# LANGUAGE DataKinds #-}

module Irc2me.Events where

import Control.Lens
import Pipes
import Control.Concurrent.Event
import Data.Time

import Irc2me.Frontend.Pipes
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Connection
import Irc2me.Database.Tables.Accounts

data AccountEvent = AccountEvent { _eventAccountId :: AccountID, _event :: Event }
  deriving (Show)

data Event
  = ClientConnected IrcHandler
  deriving (Show)

newtype IrcHandler
  = IrcHandler { runIrcHandler :: (UTCTime, IrcMessage) -> IO () }

instance Show IrcHandler where
  show _ = "IrcHandler{ (UTCTime, IrcMessage) -> IO () }"

type EventRW m = EventT RW AccountEvent m
type EventWO m = EventT WO AccountEvent m

--------------------------------------------------------------------------------
-- IRC events

clientConnected
  :: ClientConnection c
  => AccountID
  -> c
  -> AccountEvent
clientConnected aid c = AccountEvent aid $ ClientConnected $
  IrcHandler $ \(_t,msg) -> do
    let msg' = emptyServerMessage & serverIrcMessage .~ [msg]
    runEffect $ yield msg' >-> encodeMsg >-> send
 where
  send = await >>= lift . sendToClient c
