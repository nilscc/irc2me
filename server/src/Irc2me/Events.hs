{-# LANGUAGE DataKinds #-}

module Irc2me.Events where

import Control.Lens
import Pipes
import Control.Concurrent.Event
import Data.Time
import Data.Time.Clock.POSIX

import Irc2me.Frontend.Pipes
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Connection.Types
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks

data AccountEvent = AccountEvent { _eventAccountId :: AccountID, _event :: Event }
  deriving (Show)

data Event
  = ClientConnected IrcHandler
  -- | SendMessage  NetworkID IrcMessage
  deriving (Show)

type IrcMessage = (ChatMessage, Parameters)

newtype IrcHandler
  = IrcHandler { runIrcHandler :: NetworkID -> (UTCTime, IrcMessage) -> IO () }

instance Show IrcHandler where
  show _ = "IrcHandler{ (UTCTime, ChatMessage) -> IO () }"

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
  IrcHandler $ \(NetworkID nid) (t,(msg,params)) -> do

    let -- timestamp in epoch seconds
        epoch     = floor (utcTimeToPOSIXSeconds t)

        -- add timestamp
        msg'      = msg & messageTimestamp .~ Just epoch

        -- put in network message
        network   = emptyNetwork & networkId .~ Just (fromIntegral nid)
                                 & networkMessages .~ [msg']

        -- final server message
        serverMsg = emptyServerMessage & serverNetworks .~ [network]

    -- send message
    runEffect $ yield serverMsg >-> encodeMsg >-> send

 where
  send = await >>= lift . sendToClient c

--sendMessage :: AccountID -> NetworkID -> ChatMessage -> AccountEvent
--sendMessage aid nid msg = AccountEvent aid $ SendMessage nid msg
