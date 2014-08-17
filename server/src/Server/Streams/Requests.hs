{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Streams.Requests where

import Control.Lens.Operators
import Control.Monad

import Database.Query
import Database.Tables.Accounts
import Database.Tables.Networks

import IRC.Types (Identity(..))

import ProtoBuf.Types
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
       sendNewIdentity

  , do identids <- messageField ident_remove
       guard $ not (null identids)
       deleteIdentities identids

  , do idents <- messageField ident_set
       guard $ not (null idents)
       setIdentities idents
  ]

sendNewIdentity :: ServerResponse
sendNewIdentity = withAccount $ \acc -> do
  qres <- runUpdate $ addIdentity acc emptyIdent
  case qres of
    Left err       -> throwS "sendNewIdentity" $ "Unexpected SQL error: " ++ show err
    Right Nothing  -> throwS "sendNewIdentity" $ "The impossible happened."
    Right (Just i) -> do
      return $ responseOkMessage
             & ident_list .~~ [emptyIdentity i]
 where
  emptyIdent = Identity
    { ident_id = 0
    , ident_nick = ""
    , ident_nick_alt = []
    , ident_name = ""
    , ident_realname = ""
    }

sendIdentities :: ServerResponse
sendIdentities = withAccount $ \acc -> do
  qres <- runQuery $ selectIdentities acc
  case qres of
    Left err     -> throwS "sendIdentities" $ "Unexpected SQL error: " ++ show err
    Right idents -> do
      return $ responseOkMessage
             & ident_list .~~ map encodeIdentity idents

deleteIdentities :: [ID_T] -> ServerResponse
deleteIdentities identids = withAccount $ \acc -> do

  qres <- mapM (runUpdate . deleteIdentity acc . fromIntegral) identids

  if all (Right True ==) qres
    then return responseOkMessage
    else return $ responseErrorMessage $ Just "Invalid identity delete."

setIdentities :: [PB_Identity] -> ServerResponse
setIdentities idents = withAccount $ \acc -> do

  let idents' = map decodeIdentity idents
  qres <- mapM (runUpdate . setIdentity acc) idents'

  if any (Right True ==) qres
    then return $ responseOkMessage
                & ident_list .~~ [ ident | (Right True, ident) <- zip qres idents ]
    else return $ responseErrorMessage $ Just "Invalid identity set."


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
