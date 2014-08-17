module Server.Streams.Server where

import Control.Applicative
import Control.Lens.Operators
import Control.Monad

import Data.ProtocolBuffers

import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server

import Server.Response
import Server.Streams
import Server.Streams.Authenticate
import Server.Streams.Updates         ()
import Server.Streams.Requests
import Server.Streams.System

serverStream :: Stream ()
serverStream = do

  account <- authenticate <|> throwUnauthorized

  sendMessage responseOkMessage

  forever $ do

    msg <- getMessage

    let state = ServerReaderState { connectionAccount = Just account
                                  , clientMessage     = msg }

    response <- getServerResponse state $ do
                  choice [ systemStream
                         , requestStream
                         , updateStream
                         ]

    sendMessage $ addResponseId msg response

 where
  addResponseId msg response =
    response & server_response_id.field .~ (msg ^. client_response_id.field)

requestStream :: ServerResponse
requestStream = choice
  [ identityStream
  , networksStream
  ]

updateStream ::  ServerResponse
updateStream = throwS "updateStream" "Not implemented."
