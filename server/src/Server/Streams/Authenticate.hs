{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Server.Streams.Authenticate where

import Control.Lens.Operators

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Irc2me.Database.Query
import Irc2me.Database.Tables.Accounts

import Irc2me.ProtoBuf.Messages.Client
import Irc2me.ProtoBuf.Messages.Server

import Irc2me.ProtoBuf.Streams

authenticate :: Stream AccountID
authenticate = do

  msg <- getMessage

  case msg of

    _ | Just login <- msg ^. authLogin
      , Just pw    <- msg ^. authPassword -> do

        -- run database query
        maccount <- showS "authenticate" $ runQuery $ selectAccountByLogin (Text.unpack login)
        case maccount of

          Just account -> do

            ok <- showS "authenticate" $ runQuery $ checkPassword account (TE.encodeUtf8 pw)
            if ok then
              return account
             else
              throwS "authenticate" $ "Invalid password for user: " ++ Text.unpack login

          Nothing -> throwS "authenticate" $ "Invalid login: " ++ Text.unpack login

      | otherwise ->
        throwS "authenticate" $ "Unexpected message: " ++ show msg

throwUnauthorized :: Stream a
throwUnauthorized = do

  sendMessage $ responseErrorMessage $ Just "Invalid login/password."

  throwS "unauthorized" "Invalid login/password."
