module Irc2me.Frontend.Streams.Send where

--import Control.Monad.Reader
import Control.Lens

import Control.Concurrent.Event

import Irc2me.Frontend.Messages

--import Irc2me.Frontend.Streams.StreamT
import Irc2me.Frontend.Streams.Helper
import Irc2me.Events as Events

sendStream :: ServerResponse
sendStream = withMessageField clSendMessage $ \send -> do

  raiseEvent $ Events.sendMessage send

