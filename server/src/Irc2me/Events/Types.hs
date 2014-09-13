{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Irc2me.Events.Types where

import Control.Monad.Reader
import Control.Concurrent.STM

-- import Data.Coerce

import System.IO

-- lens
import Control.Lens.TH

import Irc2me.Database.Tables.Accounts

data Event
  = ClientConnected Handle
  | Stuff String
  | Something
  deriving (Eq, Show)

data AccountEvent = AccountEvent
  { _accountId :: AccountID
  , _event     :: Event
  }
  deriving (Eq, Show)

makeLenses ''AccountEvent

newtype EventQueue (mode :: ReadMode) = EventQueue (TChan AccountEvent)

type EventT (mode :: ReadMode) m = ReaderT (EventQueue mode) m

data ReadMode
  = RW  -- ^ Read/write access
  | WO  -- ^ Write only

class RunEventT (mode :: ReadMode) where
  runEventT :: MonadIO m => EventQueue WO -> EventT mode m a -> m a

instance RunEventT WO where
  runEventT = flip runReaderT

instance RunEventT RW where
  runEventT (EventQueue wo) et = do
    rw <- liftIO $ atomically $ dupTChan wo
    runReaderT et (EventQueue rw)
