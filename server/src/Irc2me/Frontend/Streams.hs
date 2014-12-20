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

import Irc2me.Events as Events
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Streams.StreamT as Stream
import Irc2me.Frontend.Streams.Helper  as Stream

-- specific streams
import Irc2me.Frontend.Streams.Authenticate
import Irc2me.Frontend.Streams.Irc
import Irc2me.Frontend.Streams.System

serverStream :: Stream ()
serverStream = do

  account <- authenticate <|> throwUnauthorized

  Stream.sendMessage responseOkMessage

  withClientConnection $ \con ->
    raiseEvent $ clientConnected account con

  forever $ do

    msg <- getMessage

    let state = ServerReaderState { connectionAccount = Just account
                                  , clientMessage     = msg }

    response <- getServerResponse state $ do
                  choice [ throwS "serverStream" $ "Not implemented: " ++ show msg
                         ]

    Stream.sendMessage $ addResponseId msg response

 where
  addResponseId msg response =
    response & serverResponseID .~ (msg ^. clientResponseID)
