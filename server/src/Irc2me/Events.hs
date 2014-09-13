{-# LANGUAGE DataKinds #-}

module Irc2me.Events
  ( -- * The `EventT` type
    EventT
  , runEventT, runEventTWO, runEventTRW
  , EventQueue, newEventQueue
    -- ** Interacting with events
  , raiseEvent, raiseEvent'
  , withEvents, withEvents'
    -- ** STM functions
  , getEvent, getEventIO
  , putEvent
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader

import Irc2me.Events.Types

--------------------------------------------------------------------------------
-- EventT

-- | `runEventT` with fixed `WO` type
runEventTWO :: MonadIO m => EventQueue WO -> EventT WO m a -> m a
runEventTWO = runEventT

-- | `runEventT` with fixed `RW` type
runEventTRW :: MonadIO m => EventQueue WO -> EventT RW m a -> m a
runEventTRW = runEventT

newEventQueue :: MonadIO m => m (EventQueue WO)
newEventQueue = do
  bc <- liftIO $ newBroadcastTChanIO
  return $ EventQueue bc

--------------------------------------------------------------------------------
-- Interacting

raiseEvent :: MonadIO m => AccountEvent -> EventT mode m ()
raiseEvent ae = do
  eq <- ask
  raiseEvent' eq ae

raiseEvent' :: MonadIO m => (EventQueue mode) -> AccountEvent -> m ()
raiseEvent' eq ae = liftIO . atomically $ putEvent eq ae

withEvents
  :: MonadIO m
  => (AccountEvent -> EventT RW m ())
  -> EventT RW m ()
withEvents go = withEvents' $ \ae -> go ae >> return True

withEvents'
  :: MonadIO m
  => (AccountEvent -> EventT RW m Bool)  -- ^ True = continue to loop
  -> EventT RW m ()
withEvents' go = do
  eq <- ask
  fix $ \loop -> do
    ae <- liftIO $ getEventIO eq
    continue <- go ae
    when continue loop

--------------------------------------------------------------------------------
-- STM functions on AccountEvent

putEvent :: EventQueue mode -> AccountEvent -> STM ()
putEvent (EventQueue eq) = writeTChan eq

getEvent :: EventQueue RW -> STM AccountEvent
getEvent (EventQueue rw) = readTChan rw

getEventIO :: EventQueue RW -> IO AccountEvent
getEventIO = liftIO . atomically . getEvent
