module Irc2me.Events.Types where

import Control.Monad.Reader
import Control.Concurrent.STM

import System.IO

import Irc2me.Database.Tables.Accounts

data AccountEvent = AccountEvent
  { _accountId :: AccountID
  , _event     :: Event
  }
  deriving (Eq, Show)

data Event
  = ClientConnected Handle
  | Stuff
  deriving (Eq, Show)

newtype EventQueue = EventQueue (TChan AccountEvent)

type EventT m = ReaderT EventQueue m
