module Irc2me.Events where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader

import Irc2me.Events.Types

--------------------------------------------------------------------------------
-- EventT

newEventQueue :: MonadIO m => m EventQueue
newEventQueue = do
  bc <- liftIO $ newBroadcastTChanIO
  return $ EventQueue bc

runEventT :: MonadIO m => EventQueue -> EventT m a -> m a
runEventT = flip runReaderT

raiseEvent :: MonadIO m => AccountEvent -> EventT m ()
raiseEvent ae = do
  eq <- ask
  raiseEvent' eq ae

raiseEvent' :: MonadIO m => EventQueue -> AccountEvent -> m ()
raiseEvent' (EventQueue c) ae = liftIO . atomically $ writeTChan c ae

withEvents
  :: MonadIO m
  => (AccountEvent -> EventT m ())
  -> EventT m ()
withEvents go = withEvents' $ \ae -> go ae >> return True

withEvents'
  :: MonadIO m
  => (AccountEvent -> EventT m Bool)  -- ^ True = continue to loop
  -> EventT m ()
withEvents' go = do
  EventQueue bc <- ask
  -- dup chan to be able to read from broadcast chan
  c  <- liftIO . atomically $ dupTChan bc
  fix $ \loop -> do
    ae <- liftIO . atomically $ readTChan c
    continue <- go ae
    when continue loop

--------------------------------------------------------------------------------
-- testing

test :: IO ()
test = do
  eq <- newEventQueue

  tid <- forkIO $ runEventT eq $ withEvents $ \ae -> do
    liftIO $ print ae

  fix $ \loop -> do
    l <- getLine
    if (null l) then do
      raiseEvent' eq (AccountEvent 0 Stuff)
      loop
     else
      killThread tid
