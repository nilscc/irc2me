{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Response where

import Control.Lens
import Control.Monad.Reader

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.ProtocolBuffers

import Database.Tables.Accounts
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server
import Server.Streams

data ServerReaderState = ServerReaderState
  { connectionAccount :: Maybe Account
  , clientMessage     :: PB_ClientMessage
  }

type ServerResponseT = StreamT (First String) (ReaderT ServerReaderState IO)
type ServerResponse  = ServerResponseT PB_ServerMessage

getServerResponse
  :: ServerReaderState -> ServerResponse -> Stream PB_ServerMessage
getServerResponse state resp =
  liftMonadTransformer (runReaderT `flip` state) resp

withAccount :: (Account -> ServerResponse) -> ServerResponse
withAccount f = do
  macc <- lift $ asks connectionAccount
  case macc of
    Nothing -> throwS "withAccount" "Login required"
    Just acc -> f acc

getClientMessage :: ServerResponseT PB_ClientMessage
getClientMessage = lift $ asks clientMessage

messageField
  :: (HasField a)
  => Getter PB_ClientMessage a
  -> ServerResponseT (FieldType a)
messageField lns = do
  msg <- getClientMessage
  return $ msg ^. lns . field

guardMessageField
  :: (HasField a, FieldType a ~ Maybe t)
  => Getter PB_ClientMessage a
  -> ServerResponseT ()
guardMessageField lns = do
  msg <- getClientMessage
  guard $ isJust $ msg ^. lns . field

guardMessageFieldValue
  :: (HasField a, Eq (FieldType a))
  => Getter PB_ClientMessage a
  -> FieldType a
  -> ServerResponseT ()
guardMessageFieldValue lns val = do
  msg <- getClientMessage
  guard $ (msg ^. lns . field) == val

requireMessageField
  :: (HasField a, FieldType a ~ Maybe t)
  => Getter PB_ClientMessage a
  -> ServerResponseT t
requireMessageField lns = do
  msg <- getClientMessage
  case msg ^. lns . field of
    Just t  -> return t
    Nothing -> mzero

requireMessageFieldValue
  :: (HasField a, FieldType a ~ Maybe t, Eq t)
  => Getter PB_ClientMessage a
  -> t
  -> ServerResponseT ()
requireMessageFieldValue lns val = do
  msg <- getClientMessage
  case msg ^. lns . field of
    Just t | t == val -> return ()
    _                 -> mzero

foldOn
  :: (Foldable f, HasField field, FieldType field ~ f a)
  => Getter PB_ClientMessage field
  -> Fold a b
  -> ServerResponseT [b]
foldOn lns fld = do
  msg <- getClientMessage
  return $ (msg ^. lns.field) ^.. folded . fld

rfoldOn
  :: (Foldable f, HasField field, FieldType field ~ f a)
  => Getter PB_ClientMessage field
  -> ReifiedFold a b
  -> ServerResponseT [b]
rfoldOn lns rfld = foldOn lns (runFold rfld)
