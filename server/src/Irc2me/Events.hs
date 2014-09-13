{-# LANGUAGE DataKinds #-}

module Irc2me.Events
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

import Control.Concurrent.STM
import Control.Monad.Reader

import Irc2me.Events.Types

--------------------------------------------------------------------------------
-- EventT

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
