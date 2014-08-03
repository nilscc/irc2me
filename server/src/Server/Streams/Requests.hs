{-# LANGUAGE PatternGuards #-}

module Server.Streams.Requests where

import Control.Lens.Operators
import Data.ProtocolBuffers

import Database.Query
import Database.Tables.Networks

import ProtoBuf.Helper
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Network
import ProtoBuf.Messages.Server

import Server.Streams
import Server.Response

networksStream :: ServerResponse
networksStream = do

  msg <- getClientMessage

  case msg of

    _ | Just True <- msg ^. network_get_list . field ->
        sendNetworks

      | otherwise ->
        throwS "networksStream" "Expected 'network_get_list' field"

sendNetworks :: ServerResponse
sendNetworks = withAccount $ \acc -> do
  qres <- runQuery $ selectNetworks acc
  case qres of
    Left err    -> throwS "sendNetworks" $ "Unexpected SQL error: " ++ show err
    Right netws ->
      return $ responseOkMessage
                 & network_list .~~ map encodeNetwork netws
