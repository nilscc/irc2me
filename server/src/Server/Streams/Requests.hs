{-# LANGUAGE PatternGuards #-}

module Server.Streams.Requests where

import Control.Monad
import Data.ProtocolBuffers

import Database.Query
import Database.Tables.Networks

import ProtoBuf.Messages.Client
--import ProtoBuf.Messages.Network
--import ProtoBuf.Messages.Server

import Server.Streams
import Server.Response

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
                   mzero
