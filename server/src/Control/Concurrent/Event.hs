{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Concurrent.Event
  ( -- * The `EventT` type
    EventT, ReadMode (..)
  , runEventT
  , runEventTWO, runEventTRW, liftWO

    -- ** The `EventQueue` type
  , EventQueue, newEventQueue

    -- * Interacting with events
  , MonadEventR (..), MonadEventW (..)
  , raiseEvent'
  , withEvents, withEvents'

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

instance MonadError err m => MonadError err (EventT mode ev m) where
  throwError = EventT . throwError
  catchError m h = EventT $
    catchError (unEventT m)
               (\e -> unEventT (h e))

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

class MonadEventW m e where
  raiseEvent :: e -> m ()
  getEventQueue :: m (EventQueue WO e)

instance MonadIO m => MonadEventW (EventT mode e m) e where
  raiseEvent ae = EventT $ do
    eq <- ask
    raiseEvent' eq ae
  getEventQueue = EventT $ do
    EventQueue eq <- ask
    return $ EventQueue eq

instance (Monad m, MonadEventW m e) => MonadEventW (ExceptT exc m) e where
  raiseEvent e = lift $ raiseEvent e
  getEventQueue = lift $ getEventQueue

class MonadEventR m e where
  getEvent :: m e

instance MonadIO m => MonadEventR (EventT RW e m) e where
  getEvent = EventT $ do
    eq <- ask
    liftIO $ readEventIO eq

instance (Monad m, MonadEventR m e) => MonadEventR (ExceptT exc m) e where
  getEvent = lift getEvent

raiseEvent' :: MonadIO m => (EventQueue mode event) -> event -> m ()
raiseEvent' eq ae = liftIO . atomically $ writeEvent eq ae

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
