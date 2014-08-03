{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Server.Streams.Authenticate where

import Control.Lens.Operators

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Data.ProtocolBuffers

import Database.Query
import Database.Tables.Accounts

import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server

import Server.Streams

authenticate :: Stream Account
authenticate = do

  msg <- getMessage

  case msg of

    _ | Just login <- msg ^. auth_login . field
      , Just pw    <- msg ^. auth_password . field -> do

        -- run database query
        maccount <- runQuery $ selectAccountByLogin (Text.unpack login)
        case maccount of

          Right (Just account) -> do

            res <- runQuery $ checkPassword account (TE.encodeUtf8 pw)
            case res of
              Right True -> return account
              _          -> throwS "authenticate" $ "Invalid password for user: " ++ Text.unpack login

          Right Nothing -> throwS "authenticate" $ "Invalid login: " ++ Text.unpack login
          Left  sqlerr  -> throwS "authenticate" $ "SqlError: " ++ show sqlerr

      | otherwise ->
        throwS "authenticate" $ "Unexpected message: " ++ show msg

throwUnauthorized :: Stream a
throwUnauthorized = do

  sendMessage $ responseErrorMessage $ Just "Invalid login/password."

  throwS "unauthorized" "Invalid login/password."
