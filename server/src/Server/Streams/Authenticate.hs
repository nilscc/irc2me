{-# LANGUAGE OverloadedStrings #-}

module Server.Streams.Authenticate where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Data.ProtocolBuffers

import Database.Query
import Database.Tables.Accounts

import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server

import Server.Streams

getClientMessage :: Stream PB_ClientMessage
getClientMessage = getMessage

authenticate :: Stream PB_ServerMessage
authenticate = do

  msg <- getClientMessage
  case msg of

    _ | Just login <- getField $ auth_login msg
      , Just pw    <- getField $ auth_login msg -> do

        -- run database query
        res <- runQuery $ checkPassword (Text.unpack login) (TE.encodeUtf8 pw)
        case res of
          Right True -> return $ responseOkMessage
          _          -> return $ responseErrorMessage (Just "Invalid user name/password")

      | otherwise ->
        throwS "authenticate" $ "Unexpected message: " ++ show msg

