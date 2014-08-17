{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Response where

import Control.Lens.Getter
import Control.Monad.Reader

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
messageField lens = do
  msg <- getClientMessage
  return $ msg ^. lens . field

guardMessageField
  :: (HasField a, FieldType a ~ Maybe t)
  => Getter PB_ClientMessage a
  -> ServerResponseT ()
guardMessageField lens = do
  msg <- getClientMessage
  guard $ isJust $ msg ^. lens . field

guardMessageFieldValue
  :: (HasField a, Eq (FieldType a))
  => Getter PB_ClientMessage a
  -> FieldType a
  -> ServerResponseT ()
guardMessageFieldValue lens val = do
  msg <- getClientMessage
  guard $ (msg ^. lens . field) == val

requireMessageField
  :: (HasField a, FieldType a ~ Maybe t)
  => Getter PB_ClientMessage a
  -> ServerResponseT t
requireMessageField lens = do
  msg <- getClientMessage
  case msg ^. lens . field of
    Just t  -> return t
    Nothing -> mzero

requireMessageFieldValue
  :: (HasField a, FieldType a ~ Maybe t, Eq t)
  => Getter PB_ClientMessage a
  -> t
  -> ServerResponseT ()
requireMessageFieldValue lens val = do
  msg <- getClientMessage
  case msg ^. lens . field of
    Just t | t == val -> return ()
    _                 -> mzero
