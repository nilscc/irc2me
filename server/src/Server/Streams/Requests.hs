{-# LANGUAGE PatternGuards #-}

module Server.Streams.Requests where

import Control.Lens.Operators
import Control.Monad

import Database.Query
import Database.Tables.Accounts
import Database.Tables.Networks

import ProtoBuf.Helper
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Identity
import ProtoBuf.Messages.Network
import ProtoBuf.Messages.Server

import Server.Streams
import Server.Response

--
-- Identities
--

identityStream :: ServerResponse
identityStream = choice

  [ do guardMessageField ident_get_all
       sendIdentities
  , do guardMessageField ident_get_new
       throwS "identityStream" "ident_get_new not implemented."
  ]

sendIdentities :: ServerResponse
sendIdentities = withAccount $ \acc -> do
  qres <- runQuery $ selectIdentities acc
  case qres of
    Left err     -> throwS "sendIdentities" $ "Unexpected SQL error: " ++ show err
    Right idents -> do
      return $ responseOkMessage
             & ident_list .~~ map encodeIdentities idents

--
-- Networks
--

networksStream :: ServerResponse
networksStream = choice

  [ do guardMessageField network_get_all_names
       sendNetworks

  , do guardMessageField network_get_new
       throwS "networksStream" "network_get_new not implemented."

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
