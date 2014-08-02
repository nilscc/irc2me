module Server.Streams.Server where

import Control.Applicative
import Control.Monad

import ProtoBuf.Messages.Server

import Server.Response
import Server.Streams
import Server.Streams.Authenticate
import Server.Streams.Updates         ()
import Server.Streams.Requests

serverStream :: Stream ()
serverStream = do

  account <- authenticate <|> throwUnauthorized

  sendMessage responseOkMessage

  let state = ServerReaderState { connectionAccount = Just account }

  forever $ do

    response <- getServerResponse state $ do
                  choice [ requestStream
                         , updateStream
                         ]

    sendMessage response

requestStream :: ServerResponse
requestStream = choice
  [ networksStream
  ]

updateStream ::  ServerResponse
updateStream = throwS "updateStream" "Not implemented."
