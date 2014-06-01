module Server.Streams.Requests where

import Server.Streams

import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Network
import ProtoBuf.Messages.Server

import Database.Tables.Networks

networksStream :: ServerResponse
networksStream = do

  msg <- getMessage
  case msg of

    _ | Just True <- getField $ network_get_list msg ->
        sendNetworks

      | otherwise ->
        throwS "networksStream" "Expected 'network_get_list' field"

 where

  sendNetworks = withAccount $ \acc -> do
                   _nets <- runQuery $ selectNetworks acc
                   undefined
