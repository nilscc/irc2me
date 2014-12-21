module Irc2me.Frontend.Streams
  ( -- * StreamT
    Stream, StreamT
  , runStream, runStreamT
    -- * Streams
  , serverStream
  ) where

import Control.Applicative
import Control.Concurrent.Event
import Control.Monad

-- lens
import Control.Lens
import Data.Text.Lens

-- local
import Irc2me.Events as Events
import Irc2me.Frontend.Messages
import Irc2me.Frontend.Streams.StreamT as Stream
import Irc2me.Frontend.Streams.Helper  as Stream

-- specific streams
import Irc2me.Frontend.Streams.Authenticate
import Irc2me.Frontend.Streams.SendMessage

serverStream :: Stream ()
serverStream = do

  account <- authenticate <|> throwUnauthorized

  Stream.sendMessage responseOkMessage

  withClientConnection $ \con ->
    raiseEvent $ clientConnected account con

  forever $ do

    msg <- getMessage

    let state = ServerReaderState { connectionAccount = Just account
                                  , responseContext   = msg }

    response <- getServerResponse state $ handleThrowS $ do
                  choice [ sendMessageResponse
                         ]

    Stream.sendMessage $ addResponseId msg response

 where
  addResponseId msg response =
    response & serverResponseID .~ (msg ^. clientResponseID)

  handleThrowS st = catchS st $ \e ->
    return $ responseErrorMessage (e ^? _Just . re _Text)
