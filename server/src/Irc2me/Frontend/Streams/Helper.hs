{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Frontend.Streams.Helper where

import Control.Concurrent.Event
import Control.Lens
import Control.Monad.Reader

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.ProtocolBuffers

import Data.Serialize
import Data.ProtocolBuffers.Internal

import qualified Data.ByteString as B

import Irc2me.Database.Tables.Accounts
import Irc2me.Events
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

sendMessage :: Encode a => a -> Stream ()
sendMessage msg = do
  let encoded = runPut $ encodeMessage msg
  sendChunk $ runPut $ putVarintPrefixedBS encoded

--------------------------------------------------------------------------------
-- Server response state

data ServerReaderState ctxt = ServerReaderState
  { connectionAccount :: Maybe AccountID
  , responseContext   :: ctxt
  }

type ServerResponseT ctxt
  = StreamT (First String)
            (ReaderT (ServerReaderState ctxt)
                     (EventT WO AccountEvent IO))

type ServerResponse = ServerResponseT ClientMessage ServerMessage

getServerResponse
  :: ServerReaderState ClientMessage
  -> ServerResponse
  -> Stream ServerMessage
getServerResponse state resp =
  liftMonadTransformer (runReaderT `flip` state) resp

withAccount :: (AccountID -> ServerResponseT ctxt a) -> ServerResponseT ctxt a
withAccount f = do
  macc <- lift $ asks connectionAccount
  case macc of
    Nothing -> throwS "withAccount" "Login required"
    Just acc -> f acc

withResponseContext
  :: (ctxt' -> ctxt)
  -> ServerResponseT ctxt  a
  -> ServerResponseT ctxt' a
withResponseContext f = mapStreamT $
  withReaderT (\rs -> rs { responseContext = f (responseContext rs) })

setResponseContext :: ctxt -> ServerResponseT ctxt a -> ServerResponseT ctxt' a
setResponseContext ctxt = mapStreamT $
  withReaderT (\rs -> rs { responseContext = ctxt})

getResponseContext :: ServerResponseT ctxt ctxt
getResponseContext = lift $ asks responseContext

getClientMessage :: ServerResponseT ClientMessage ClientMessage
getClientMessage = getResponseContext

messageField
  :: Getter ctxt a
  -> ServerResponseT ctxt a
messageField lns = do
  msg <- getResponseContext
  return $ msg ^. lns

guardMessageField
  :: Getter ctxt (Maybe a)
  -> ServerResponseT ctxt ()
guardMessageField lns = do
  msg <- getResponseContext
  guard $ isJust $ msg ^. lns

guardMessageFieldValue
  :: Eq a
  => Getter ctxt a
  -> a
  -> ServerResponseT ctxt ()
guardMessageFieldValue lns val = do
  msg <- getResponseContext
  guard $ (msg ^. lns) == val

requireMessageField
  :: Getter ctxt (Maybe t)
  -> ServerResponseT ctxt t
requireMessageField lns = do
  msg <- getResponseContext
  case msg ^. lns of
    Just t  -> return t
    Nothing -> mzero

requireMessageFieldValue
  :: Eq t
  => Getter ctxt (Maybe t)
  -> t
  -> ServerResponseT ctxt ()
requireMessageFieldValue lns val = do
  msg <- getResponseContext
  case msg ^. lns of
    Just t | t == val -> return ()
    _                 -> mzero

------------------------------------------------------------------------------
-- Folds

foldOn, guardFoldOn
  :: Foldable f
  => Getter ctxt (f a)
  -> Fold a b
  -> ServerResponseT ctxt [b]

foldOn lns fld = do
  msg <- getResponseContext
  return $ (msg ^. lns) ^.. folded . fld

guardFoldOn lns fld = do
  lis <- foldOn lns fld
  guard $ not $ null lis
  return lis

foldROn, guardFoldROn
  :: Foldable f
  => Getter ctxt (f a)
  -> ReifiedFold a b
  -> ServerResponseT ctxt [b]

foldROn lns rfld = foldOn lns (runFold rfld)

guardFoldROn lns rfld = do
  lis <- foldROn lns rfld
  guard $ not $ null lis
  return lis
