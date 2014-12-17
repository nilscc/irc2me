{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Irc2me.Frontend.Streams.Helper where

import Control.Concurrent.Event
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except

--import Data.Foldable
--import Data.Maybe
import Data.Monoid
import Data.ProtocolBuffers

import Data.Serialize
import Data.ProtocolBuffers.Internal

import qualified Data.ByteString as B

import Irc2me.Database.Tables.Accounts
import Irc2me.Events.Types
import Irc2me.Frontend.Connection.Types
import Irc2me.Frontend.Messages.Client
import Irc2me.Frontend.Messages.Server
import Irc2me.Frontend.Streams.StreamT

--------------------------------------------------------------------------------
-- messages

getMessage :: (Monad m, Decode a) => StreamT (First String) m a
getMessage = withChunks $ \chunks ->

  handleChunks chunks $ runGetPartial getVarintPrefixedBS

 where

  handleChunks (chunk : rest) f

    -- skip empty chunks
    | B.null chunk = handleChunks rest f

    | otherwise =

      -- parse chunk
      case f chunk of

        Fail err _ -> throwS "getMessage" $ "Unexpected error: " ++ show err

        Partial f' -> handleChunks rest f'

        Done bs chunk' -> do
          -- try to parse current message
          case runGet decodeMessage bs of
            Left err  -> throwS "getMessage" $ "Failed to parse message: " ++ show err
            Right msg -> return (chunk' : rest, msg)

  handleChunks [] f =

    case f B.empty of
      Done bs _ | Right msg <- runGet decodeMessage bs ->
        return ([], msg)
      _ -> throwS "getMessage" "Unexpected end of input."

sendMessage
  :: (MonadIO m, ClientConnection con, Encode a)
  => con -> a -> m ()
sendMessage con msg = do
  let encoded = runPut $ encodeMessage msg
  sendChunk con $ runPut $ putVarintPrefixedBS encoded

--------------------------------------------------------------------------------
-- Server response state

data ServerReaderState = ServerReaderState
  { connectionAccount :: Maybe AccountID
  , clientMessage     :: ClientMessage
  }

type ServerResponseT r
  = StreamT (First String)
            (ReaderT r
                     (EventT WO AccountEvent IO))

newtype AsyncResponse m = AsyncResponse { sendAsyncMessage :: ServerMessage -> m () }

type ServerResponse = ServerResponseT ClientMessage (Either ServerMessage (AsyncResponse IO))

class HasOptionalMessageField a b | a -> b where
  hasMessageField :: Getter r a -> ServerResponseT r b

instance HasOptionalMessageField (Maybe a) a where
  hasMessageField g = maybe mzero return =<< lift (asks (^. g))

instance HasOptionalMessageField [a] [a] where
  hasMessageField g = do
    l <- lift $ asks (^. g)
    guard $ not $ null l
    return l

withResponseT
  :: (b -> a)
  -> ServerResponseT a s
  -> ServerResponseT b s
withResponseT f (StreamT st) = StreamT $ \s ->
  mapExceptT (withReaderT f) (st s)

withMessageField
  :: HasOptionalMessageField a b
  => Getter r a
  -> (b -> ServerResponseT b s)
  -> ServerResponseT r s
withMessageField g f = do
  b <- hasMessageField g
  withResponseT (const b) (f b)

withMessageField_
  :: HasOptionalMessageField a b
  => Getter r a
  -> ServerResponseT b s
  -> ServerResponseT r s
withMessageField_ g st = withMessageField g (const st)
