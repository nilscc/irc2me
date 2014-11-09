module Irc2me.ProtoBuf.Streams where

import Control.Applicative
import Control.Lens.Operators
import Control.Monad

import Irc2me.ProtoBuf.Messages

import Irc2me.ProtoBuf.Streams.StreamT
import Irc2me.ProtoBuf.Streams.Helper
import Irc2me.ProtoBuf.Streams.Authenticate

serverStream :: Stream ()
serverStream = do

  account <- authenticate <|> throwUnauthorized

  sendMessage responseOkMessage

  forever $ do

    msg <- getMessage

    let state = ServerReaderState { connectionAccount = Just account
                                  , clientMessage     = msg }

    response <- getServerResponse state $ do
                  choice [ ]

    sendMessage $ addResponseId msg response

 where
  addResponseId msg response =
    response & serverResponseID .~ (msg ^. clientResponseID)
