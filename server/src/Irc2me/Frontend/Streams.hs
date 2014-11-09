module Irc2me.ProtoBuf.Streams
  ( -- * StreamT
    Stream, StreamT
  , runStream, runStreamT
    -- * Streams
  , serverStream
  ) where

import Control.Applicative
import Control.Lens.Operators
import Control.Monad

import Irc2me.ProtoBuf.Messages

import Irc2me.ProtoBuf.Streams.StreamT
import Irc2me.ProtoBuf.Streams.Helper

-- specific streams
import Irc2me.ProtoBuf.Streams.Authenticate
import Irc2me.ProtoBuf.Streams.System

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
                         ]

    sendMessage $ addResponseId msg response

 where
  addResponseId msg response =
    response & serverResponseID .~ (msg ^. clientResponseID)
