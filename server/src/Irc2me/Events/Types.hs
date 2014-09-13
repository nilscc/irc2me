{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Irc2me.Events.Types where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Concurrent.STM

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

data ReadMode
  = RW  -- ^ Read/write access
  | WO  -- ^ Write only

newtype EventQueue (mode :: ReadMode) = EventQueue (TChan AccountEvent)

type role EventQueue representational

newtype EventT (mode :: ReadMode) m a = EventT
  { unEventT :: ReaderT (EventQueue mode) m a }
 deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  , MonadTrans
  , MonadCont
  , MonadFix
  , Alternative
  , MonadPlus
  , MonadError e
  , MonadState s
  , MonadWriter w
  )

instance MonadReader r m => MonadReader r (EventT mode m) where
  ask = EventT $ lift ask
  local f (EventT a) = EventT $ do
    eq <- ask
    lift $ local f (runReaderT a eq)


type role EventT representational nominal nominal

class RunEventT (mode :: ReadMode) where
  runEventT :: MonadIO m => EventQueue WO -> EventT mode m a -> m a

instance RunEventT WO where
  runEventT eq (EventT et) = runReaderT et eq

instance RunEventT RW where
  runEventT (EventQueue wo) (EventT et) = do
    rw <- liftIO $ atomically $ dupTChan wo
    runReaderT et (EventQueue rw)
