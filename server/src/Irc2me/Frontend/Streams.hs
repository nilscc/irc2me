module Irc2me.Frontend.Streams
  ( -- * StreamT
    Stream, StreamT
  , runStream, runStreamT
    -- * Streams
  , serverStream
  ) where

import Control.Applicative
import Control.Concurrent.Event
import Control.Lens.Operators
import Control.Monad

import Irc2me.Events
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Streams.StreamT
import Irc2me.Frontend.Streams.Helper

-- specific streams
import Irc2me.Frontend.Streams.Authenticate
import Irc2me.Frontend.Streams.System

serverStream :: Stream ()
serverStream = do

  account <- authenticate <|> throwUnauthorized

  sendMessage responseOkMessage

  withClientConnection $ \con ->
    raiseEvent $ clientConnected account con

  forever $ do

    msg <- getMessage

    let state = ServerReaderState { connectionAccount = Just account
                                  , clientMessage     = msg }

    response <- getServerResponse state $ do
                  choice [ systemStream
                         , throwS "serverStream" $ "Not implemented: " ++ show msg
                         ]

    sendMessage $ addResponseId msg response

 where
  addResponseId msg response =
    response & serverResponseID .~ (msg ^. clientResponseID)
