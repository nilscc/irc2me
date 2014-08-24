{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Streams.Requests where

-- lens imports
import Control.Lens hiding (Identity)
import Numeric.Lens

import Control.Monad

import Data.Maybe

import Database.Query
import Database.Tables.Accounts
import Database.Tables.Networks

import IRC.Types (Identity(..))

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

  [ sendIdentities
  , updateIdentities
  , addIdentities
  , deleteIdentities
  ]

sendIdentities :: ServerResponse
sendIdentities = withAccount $ \acc -> do

  guardMessageField ident_get_all

  qres <- runQuery $ selectIdentities acc
  case qres of
    Left err     -> throwS "sendIdentities" $ "Unexpected SQL error: " ++ show err
    Right idents -> do
      return $ responseOkMessage
             & ident_list .~~ map encodeIdentity idents

deleteIdentities :: ServerResponse
deleteIdentities = withAccount $ \acc -> do

  ids  <- guardFoldOn ident_remove $ re (integral :: Prism' Integer ID_T)
  qres <- mapM (runUpdate . deleteIdentity acc) ids

  if all (Right True ==) qres
    then return responseOkMessage
    else return $ responseErrorMessage $ Just "Invalid identity delete."

updateIdentities :: ServerResponse
updateIdentities = withAccount $ \acc -> do

  idents <- guardFoldROn ident_set identitiesWithID
  up_res <- mapM (runUpdate . setIdentity acc . decodeIdentity) idents

  return $
    if all (Right True ==) up_res
      then responseOkMessage
      else responseErrorMessage $ Just "Invalid IDENTIY SET"

addIdentities :: ServerResponse
addIdentities = withAccount $ \acc -> do

  idents <- guardFoldROn ident_set identitiesWithoutID
  in_res <- mapM (runUpdate . addIdentity acc . decodeIdentity) idents

  return $
    if all (either (const False) isJust) in_res
      then responseOkMessage &
             ident_list .~~ [ emptyIdentity i | Right (Just i) <- in_res ]
      else responseErrorMessage $ Just "Invalid IDENTIY SET"

--
-- Networks
--

networksStream :: ServerResponse
networksStream = choice

  [ do guardMessageField network_get_all_names
       sendNetworks

  , do networks <- messageField network_set
       guard $ not (null networks)
       setNetworks networks

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

setNetworks :: [PB_Network] -> ServerResponse
setNetworks _networks = withAccount $ \_acc -> do

  -- let (oldNetworks, newNetworks) = partition (hasField network_id) networks

  throwS "setNetworks" "not implemented"
