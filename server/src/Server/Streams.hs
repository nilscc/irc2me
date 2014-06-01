{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Server.Streams
  ( Stream, StreamT
  , throwS
  , runStreamOnHandle
    -- ** Messages
  , getMessage
  , sendMessage
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.Monoid
import Data.Serialize
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import System.IO

type Chunks = [B.ByteString]

--------------------------------------------------------------------------------
-- newtype on chunks

newtype StreamT e m a = StreamT { runStreamT :: (Handle, Chunks) -> ExceptT e m (Chunks, a) }

type Stream a = (Applicative m, MonadIO m) => StreamT (First String) m a

-- Instance definitions

instance Monad m => Monad (StreamT e m) where
  return a = StreamT $ \(_,c) -> return (c, a)
  m >>= n  = StreamT $ \s@(h,_) -> do
               (c', a) <- runStreamT m s
               runStreamT (n a) (h,c')

instance (Functor m, MonadIO m) => MonadIO (StreamT e m) where
  liftIO f = StreamT $ \(_,s) -> (s,) <$> liftIO f


instance Functor m => Functor (StreamT e m) where
  fmap f m = StreamT $ \s ->
    second f `fmap` runStreamT m s

instance (Functor m, Monad m) => Applicative (StreamT e m) where
  pure = return
  (<*>) = ap

instance (Functor m, Monad m, Monoid e) => Alternative (StreamT e m) where
  empty   = StreamT $ \_ -> empty
  m <|> n = StreamT $ \s -> runStreamT m s <|> runStreamT n s

instance (Functor m, Monad m, Monoid e) => MonadPlus (StreamT e m) where
  mzero     = StreamT $ \_ -> mzero
  mplus m n = StreamT $ \s -> runStreamT m s <|> runStreamT n s

--------------------------------------------------------------------------------

throwS :: Monad m => String -> String -> StreamT (First String) m a
throwS f e = StreamT $ \_ -> throwE (First $ Just $ "[" ++ f ++ "] " ++ e)

chunksFromHandle :: Handle -> IO Chunks
chunksFromHandle h = BL.toChunks <$> BL.hGetContents h

runStreamOnHandle :: (Functor m, Applicative m, MonadIO m) => Handle -> Stream a -> m (Either String a)
runStreamOnHandle h st = do
  res <- runStreamTOnHandle h st
  case res of
    Right x                     -> return $ Right x
    Left (getFirst -> Just err) -> return $ Left err
    _                           -> return $ Left "Unexpected error in 'runStreamOnHandle'"

runStreamTOnHandle :: (Functor m, MonadIO m) => Handle -> StreamT e m a -> m (Either e a)
runStreamTOnHandle h st = do
  c <- liftIO $ chunksFromHandle h
  runExceptT $ snd <$> runStreamT st (h,c)

withHandle :: (Handle -> StreamT e m a) -> StreamT e m a
withHandle f = StreamT $ \s@(h,_) -> runStreamT (f h) s

--------------------------------------------------------------------------------
-- messages

getMessage :: Decode a => Stream a
getMessage = StreamT $ \(_,chunks) ->

  handleChunks chunks $ runGetPartial getVarintPrefixedBS

 where

  handleChunks (chunk : rest) f

    -- skip empty chunks
    | B.null chunk = handleChunks rest f

    | otherwise =

      -- parse chunk
      case f chunk of

        Fail err _ -> throwE $ First . Just $ "[getMessage] Unexpected error: " ++ show err

        Partial f' -> handleChunks rest f'

        Done bs chunk' -> do
          -- try to parse current message
          case runGet decodeMessage bs of
            Left err  -> throwE $ First . Just $ "[getMessage] Failed to parse message: " ++ show err
            Right msg -> return (chunk' : rest, msg)

  handleChunks [] f =

    case f B.empty of
      Done bs _ | Right msg <- runGet decodeMessage bs ->
        return ([], msg)
      _ -> throwE $ First . Just $ "[getMessage] Unexpected end of input."

sendMessage :: Encode a => a -> Stream ()
sendMessage msg = withHandle $ \h -> do

  let encoded = runPut $ encodeMessage msg
  liftIO $ B.hPut h $ runPut $ putVarintPrefixedBS encoded