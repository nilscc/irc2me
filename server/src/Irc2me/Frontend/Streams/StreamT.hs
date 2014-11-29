{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Irc2me.Frontend.Streams.StreamT
  ( -- * Streams
    Stream, StreamT
  , Chunks
  , throwS, showS
  , choice
  , liftMonadTransformer

    -- ** Connections
  , runStream
  , runStreamT
  , withChunks, withClientConnection, sendChunk
  , disconnect
  ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.Event
import Control.Monad
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.List
import Data.Monoid

import Irc2me.Events
import Irc2me.Frontend.Connection.Types

--------------------------------------------------------------------------------
-- newtype on chunks

newtype StreamT e m a = StreamT
  { unStreamT :: ClientConnection con => (con, Chunks) -> ExceptT e m (Chunks, a)
  }

type Stream = StreamT (First String) (EventWO IO)

-- Instance definitions

instance Monad m => Monad (StreamT e m) where
  return a = StreamT $ \(_,c) -> return (c, a)
  m >>= n  = StreamT $ \s@(h,_) -> do
               (c', a) <- unStreamT m s
               unStreamT (n a) (h,c')

instance (Functor m, MonadIO m) => MonadIO (StreamT e m) where
  liftIO f = StreamT $ \(_,s) -> (s,) <$> liftIO f

instance Functor m => Functor (StreamT e m) where
  fmap f m = StreamT $ \s ->
    second f `fmap` unStreamT m s

instance (Functor m, Monad m) => Applicative (StreamT e m) where
  pure = return
  (<*>) = ap

instance (Functor m, Monad m, Monoid e) => Alternative (StreamT e m) where
  empty   = StreamT $ \_ -> empty
  m <|> n = StreamT $ \s -> unStreamT m s <|> unStreamT n s

instance (Monad m, Monoid e) => MonadPlus (StreamT e m) where
  mzero     = StreamT $ \_ -> mzero
  mplus m n = StreamT $ \s -> unStreamT m s `mplus` unStreamT n s

instance MonadTrans (StreamT e) where
  lift f = StreamT $ \(_,c) -> do
    a <- lift f
    return (c,a)

instance MonadError e m => MonadError e (StreamT e m) where
  throwError e   = StreamT $ \_ -> throwError e
  catchError s c = StreamT $ \a ->
    unStreamT s a `catchError` (\e -> unStreamT (c e) a)

instance (Monad m, Functor m, MonadEventR m ev) => MonadEventR (StreamT e m) ev where
  getEvent = StreamT $ \(_,c) -> (c,) <$> getEvent

instance (Monad m, Functor m, MonadEventW m ev) => MonadEventW (StreamT e m) ev where
  raiseEvent e = StreamT $ \(_,c) -> (c,) <$> raiseEvent e
  getEventQueue = lift getEventQueue

--------------------------------------------------------------------------------

disconnect
  :: Monad m
  => Maybe String       -- ^ optional reason
  -> StreamT (First String) m a
disconnect reason = StreamT $ \_ ->
  throwError $ First $ Just $ maybe "Disconnected." ("Disconnected: " ++) reason

throwS
  :: Monad m
  => String -- ^ origin of error
  -> String -- ^ error message
  -> StreamT (First String) m a
throwS f e = StreamT $ \_ -> throwError (First $ Just $ "[" ++ f ++ "] " ++ e)

-- | Run an `ExceptT` monad in `StreamT` and rethrow the exception as `String`
showS
  :: (Show e, Functor m, Monad m)
  => String -> ExceptT e m a -> StreamT (First String) m a
showS w et = do
  r <- lift $ runExceptT et
  case r of
    Left  e -> throwS w (show e)
    Right a -> return a

sendChunk :: MonadIO m => ByteString -> StreamT e m ()
sendChunk bs = StreamT $ \(con,c) -> do
  liftIO $ sendToClient con bs
  return (c,())

withClientConnection
  :: (Monad m)
  => (forall con. ClientConnection con => con -> StreamT e m a)
  -> StreamT e m a
withClientConnection f = StreamT $ \s@(con,_) -> unStreamT (f con) s

-- | Manually modify bytestring chunks
withChunks :: Monad m => (Chunks -> StreamT e m (Chunks, a)) -> StreamT e m a
withChunks f = StreamT $ \s@(_,c) -> do
  (_, res) <- unStreamT (f c) s
  return res

-- | Run a `Stream` monad with on a handle. Returns the first error message (if any)
runStream
  :: (MonadIO m, ClientConnection c)
  => c
  -> EventQueue WO AccountEvent
  -> Stream a
  -> m (Either String a)
runStream con q st = liftIO $ do
  res <- runEventTWO q $ runStreamT con st
  case res of
    Right x                     -> return $ Right x
    Left (getFirst -> Just err) -> return $ Left err
    Left e                      -> return $ Left $ "Unexpected error in 'runStream' (" ++ show e ++")"

-- | Generalized `runStreamOnHandle`
runStreamT
  :: (Functor m, MonadIO m, ClientConnection c)
  => c -> StreamT e m a -> m (Either e a)
runStreamT con st = do
  c <- liftIO $ incomingChunks con
  runExceptT $ snd <$> unStreamT st (con,c)

choice :: (Alternative m, Monad m, Monoid e) => [StreamT e m a] -> StreamT e m a
choice = foldl' (<|>) empty

liftMonadTransformer
  :: (MonadTrans t, Monad m)
  => (t m (Either e (Chunks, a)) -> m (Either e (Chunks, a)))
  -> StreamT e (t m) a
  -> StreamT e m a
liftMonadTransformer transf streamt = StreamT $ \s -> do
  res <- lift $ transf $ runExceptT $ unStreamT streamt s
  case res of
    Left e -> throwError e
    Right a -> return a
