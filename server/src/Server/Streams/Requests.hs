{-# LANGUAGE PatternGuards #-}

module Server.Streams.Requests where

import Control.Lens.Operators
import Control.Monad

import Database.Query
import Database.Tables.Networks

import ProtoBuf.Helper
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Network
import ProtoBuf.Messages.Server

import Server.Streams
import Server.Response

networksStream :: ServerResponse
networksStream = choice

  [ do guardMessageField network_get_list
       sendNetworks

  , do networks <- messageField network_add
       guard $ not (null networks)
       throwS "networksStream" "network_add not implemented."

  , do networks <- messageField network_remove
       guard $ not (null networks)
       throwS "networksStream" "network_remove not implemented."

  ]

sendNetworks :: ServerResponse
sendNetworks = withAccount $ \acc -> do
  qres <- runQuery $ selectNetworks acc
  case qres of
    Left err    -> throwS "sendNetworks" $ "Unexpected SQL error: " ++ show err
    Right netws ->
      return $ responseOkMessage
                 & network_list .~~ map encodeNetwork netws
