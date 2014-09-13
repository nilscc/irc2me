{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Irc2me.Events.Types
  ( -- * The `EventT` type
    EventT, ReadMode (..)
  , runEventT
  , runEventTWO, runEventTRW, liftWO

    -- * The `EventQueue` type
  , EventQueue, newEventQueue

    -- * Interacting with events
  , raiseEvent, raiseEvent'
  , getEvent, withEvents, withEvents'

    -- ** STM functions
  , readEvent, readEventIO
  , writeEvent
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Concurrent.STM

--------------------------------------------------------------------------------
-- EventT

type role EventQueue representational nominal

data ReadMode
  = RW  -- ^ Read/write access
  | WO  -- ^ Write only

newtype EventQueue (mode :: ReadMode) e = EventQueue (TChan e)

type role EventT representational nominal nominal nominal

newtype EventT (mode :: ReadMode) e m a = EventT
  { unEventT :: ReaderT (EventQueue mode e) m a }
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

instance MonadReader r m => MonadReader r (EventT mode e m) where
  ask = EventT $ lift ask
  local f (EventT a) = EventT $ do
    eq <- ask
    lift $ local f (runReaderT a eq)

class RunEventT (mode :: ReadMode) where
  runEventT :: MonadIO m => EventQueue WO e -> EventT mode e m a -> m a

instance RunEventT WO where
  runEventT eq (EventT et) = runReaderT et eq

instance RunEventT RW where
  runEventT (EventQueue wo) (EventT et) = do
    rw <- liftIO $ atomically $ dupTChan wo
    runReaderT et (EventQueue rw)

-- | `runEventT` with fixed `WO` type
runEventTWO :: MonadIO m => EventQueue WO event -> EventT WO event m a -> m a
runEventTWO = runEventT

-- | `runEventT` with fixed `RW` type
runEventTRW :: MonadIO m => EventQueue WO event -> EventT RW event m a -> m a
runEventTRW = runEventT

-- | Lift a \"write only\" action to a \"read/write\" `EventT`
liftWO :: Monad m => EventT WO event m a -> EventT RW event m a
liftWO (EventT et) = EventT $ do
  EventQueue rw <- ask
  lift $ runReaderT et (EventQueue rw)

--------------------------------------------------------------------------------
-- Event queue

newEventQueue :: MonadIO m => m (EventQueue WO event)
newEventQueue = do
  bc <- liftIO $ newBroadcastTChanIO
  return $ EventQueue bc

--------------------------------------------------------------------------------
-- Interacting

raiseEvent :: MonadIO m => event -> EventT mode event m ()
raiseEvent ae = EventT $ do
  eq <- ask
  raiseEvent' eq ae

raiseEvent' :: MonadIO m => (EventQueue mode event) -> event -> m ()
raiseEvent' eq ae = liftIO . atomically $ writeEvent eq ae

getEvent :: MonadIO m => EventT RW event m event
getEvent = EventT $ do
  eq <- ask
  liftIO $ readEventIO eq

withEvents
  :: MonadIO m
  => (event -> EventT RW event m ())
  -> EventT RW event m ()
withEvents go = withEvents' $ \ae -> EventT $ do
  unEventT (go ae)
  return True

withEvents'
  :: MonadIO m
  => (event -> EventT RW event m Bool)  -- ^ True = continue to loop
  -> EventT RW event m ()
withEvents' go = fix $ \loop -> do
  ae <- getEvent
  continue <- go ae
  when continue loop

--------------------------------------------------------------------------------
-- STM functions on AccountEvent

writeEvent :: EventQueue mode event -> event -> STM ()
writeEvent (EventQueue eq) = writeTChan eq

readEvent :: EventQueue RW event -> STM event
readEvent (EventQueue rw) = readTChan rw

readEventIO :: EventQueue RW event -> IO event
readEventIO = liftIO . atomically . readEvent
