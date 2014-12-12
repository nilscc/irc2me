{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Broadcast
  ( Broadcast
  , BroadcastT
  , startBroadcasting, startBroadcasting'
  , stopBroadcasting
  , broadcast, getBroadcastFunction
  , subscribe
  ) where

import Control.Concurrent
import Control.Exception

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Typeable

-- stm
import Control.Concurrent.STM

-- lens
import Control.Lens

data Broadcast msg = Broadcast
  { _broadcastMessages :: TChan msg
  , _broadcastThread   :: ThreadId
  }

makeLenses ''Broadcast

-- | The broadcast monad, necessary for `broadcast`
newtype BroadcastT msg m a = BroadcastT
  { unBroadcastT :: ReaderT (TChan msg) m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadTrans
    , MonadIO
    , MonadState s
    , MonadWriter w
    , MonadError e
    )

instance MonadReader r m => MonadReader r (BroadcastT msg m) where
  ask = lift ask
  local f m = BroadcastT $ do
    bc <- ask
    lift $ local f $ runReaderT (unBroadcastT m) bc

-- | STOP signal
data StopBroadcasting = StopBroadcasting
  deriving (Eq, Show, Typeable)

instance Exception StopBroadcasting

-- | Create a new broadcast
startBroadcasting
  :: BroadcastT msg IO ()
  -> IO (Broadcast msg)
startBroadcasting go = startBroadcasting' go (return ())

startBroadcasting'
  :: BroadcastT msg IO ()
  -> IO ()                    -- ^ Action to run when stopping broadcast
  -> IO (Broadcast msg)
startBroadcasting' go finalize = do

  messages <- newBroadcastTChanIO

  -- start thread
  tid <- forkIO $ do

    -- catch STOP signal
    catch (runReaderT (unBroadcastT go) messages)
          (\StopBroadcasting -> finalize)

  return $ Broadcast
    { _broadcastMessages = messages
    , _broadcastThread   = tid
    }

-- | Stop broadcasting
stopBroadcasting :: Broadcast msg -> IO ()
stopBroadcasting bc = throwTo (bc ^. broadcastThread) StopBroadcasting

-- | Send a new broadcast message to all subscribers
broadcast :: MonadIO m => msg -> BroadcastT msg m ()
broadcast msg = BroadcastT $ do
  messages <- ask
  liftIO $ atomically $ writeTChan messages msg

getBroadcastFunction :: MonadIO m => BroadcastT msg m (msg -> m ())
getBroadcastFunction = BroadcastT $ do
  messages <- ask
  return $ liftIO . atomically . writeTChan messages

-- | Receive broadcasted messages
subscribe :: Broadcast msg -> (msg -> IO ()) -> IO ()
subscribe bc go = do

  incoming <- atomically $ dupTChan (bc ^. broadcastMessages)

  safeFix $ \loop -> do
    msg <- atomically $ readTChan incoming
    go msg
    loop

 where
  safeFix f = fix f `catch` (\BlockedIndefinitelyOnSTM -> return ())
