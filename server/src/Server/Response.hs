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

import Irc2me.Database.Tables.Accounts
import Irc2me.ProtoBuf.Messages.Client
import Irc2me.ProtoBuf.Messages.Server
import Irc2me.ProtoBuf.Streams

data ServerReaderState = ServerReaderState
  { connectionAccount :: Maybe AccountID
  , clientMessage     :: ClientMessage
  }

type ServerResponseT = StreamT (First String) (ReaderT ServerReaderState IO)
type ServerResponse  = ServerResponseT ServerMessage

getServerResponse
  :: ServerReaderState -> ServerResponse -> Stream ServerMessage
getServerResponse state resp =
  liftMonadTransformer (runReaderT `flip` state) resp

withAccount :: (AccountID -> ServerResponse) -> ServerResponse
withAccount f = do
  macc <- lift $ asks connectionAccount
  case macc of
    Nothing -> throwS "withAccount" "Login required"
    Just acc -> f acc

getClientMessage :: ServerResponseT ClientMessage
getClientMessage = lift $ asks clientMessage

messageField
  :: (HasField a)
  => Getter ClientMessage a
  -> ServerResponseT (FieldType a)
messageField lns = do
  msg <- getClientMessage
  return $ msg ^. lns . field

guardMessageField
  :: (HasField a, FieldType a ~ Maybe t)
  => Getter ClientMessage a
  -> ServerResponseT ()
guardMessageField lns = do
  msg <- getClientMessage
  guard $ isJust $ msg ^. lns . field

guardMessageFieldValue
  :: (HasField a, Eq (FieldType a))
  => Getter ClientMessage a
  -> FieldType a
  -> ServerResponseT ()
guardMessageFieldValue lns val = do
  msg <- getClientMessage
  guard $ (msg ^. lns . field) == val

requireMessageField
  :: (HasField a, FieldType a ~ Maybe t)
  => Getter ClientMessage a
  -> ServerResponseT t
requireMessageField lns = do
  msg <- getClientMessage
  case msg ^. lns . field of
    Just t  -> return t
    Nothing -> mzero

requireMessageFieldValue
  :: (HasField a, FieldType a ~ Maybe t, Eq t)
  => Getter ClientMessage a
  -> t
  -> ServerResponseT ()
requireMessageFieldValue lns val = do
  msg <- getClientMessage
  case msg ^. lns . field of
    Just t | t == val -> return ()
    _                 -> mzero

------------------------------------------------------------------------------
-- Folds

foldOn, guardFoldOn
  :: (Foldable f, HasField field, FieldType field ~ f a)
  => Getter ClientMessage field
  -> Fold a b
  -> ServerResponseT [b]

foldOn lns fld = do
  msg <- getClientMessage
  return $ (msg ^. lns.field) ^.. folded . fld

guardFoldOn lns fld = do
  lis <- foldOn lns fld
  guard $ not $ null lis
  return lis

foldROn, guardFoldROn
  :: (Foldable f, HasField field, FieldType field ~ f a)
  => Getter ClientMessage field
  -> ReifiedFold a b
  -> ServerResponseT [b]

foldROn lns rfld = foldOn lns (runFold rfld)

guardFoldROn lns rfld = do
  lis <- foldROn lns rfld
  guard $ not $ null lis
  return lis
