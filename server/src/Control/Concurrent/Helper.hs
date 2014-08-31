module Control.Concurrent.Helper
  ( Concurrent, ConcurrentM
  , runConcurrent
  , newThread, waitForTermination
  ) where

import Control.Applicative
import Control.Exception

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.State

newtype ConcurrentM m a = ConcurrentM { runConcurrentM :: (StateT [TMVar ()] m a) }

type Concurrent a = ConcurrentM IO a

instance Functor f => Functor (ConcurrentM f) where
  fmap x f = ConcurrentM $ x <$> runConcurrentM f

instance (Applicative m, Monad m) => Applicative (ConcurrentM m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ConcurrentM m) where
  return x = ConcurrentM $ return x
  a >>= m  = ConcurrentM $ runConcurrentM a >>= runConcurrentM . m

instance MonadIO m => MonadIO (ConcurrentM m) where
  liftIO io = ConcurrentM $ liftIO io

runConcurrent :: MonadIO m => Concurrent a -> m a
runConcurrent (ConcurrentM c) = liftIO $ evalStateT c []

newThread :: IO () -> Concurrent ThreadId
newThread io = ConcurrentM $ do

  tmvar <- liftIO $ newEmptyTMVarIO
  tid   <- liftIO $ forkIO $ io `finally` atomically (putTMVar tmvar ())

  -- add tmvar to state
  modify $ (++ [tmvar])

  return tid

waitForTermination :: Concurrent ()
waitForTermination = ConcurrentM $ do
  s <- get
  liftIO $ atomically $ mapM_ takeTMVar s
