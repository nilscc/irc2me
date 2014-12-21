module Irc2me.Frontend.Streams.SendMessage where

import Control.Applicative

-- lens
import Control.Lens

-- local
import Control.Concurrent.Event
import Irc2me.Events as Event

import Irc2me.Database.Tables.Networks

import Irc2me.Frontend.Streams.Helper
import Irc2me.Frontend.Streams.StreamT

import Irc2me.Frontend.Messages.ChatMessage
import Irc2me.Frontend.Messages.Client
import Irc2me.Frontend.Messages.Server

sendMessage :: ServerResponse
sendMessage = do

  send <- requireMessageField clientSendMessage

  setResponseContext send $ do
    
    -- figure out message type
    ty  <- choice [ Left  <$> requireMessageField sendType
                  , Right <$> requireMessageField sendTypeRaw
                  -- default:
                  , return $ Left PRIVMSG
                  ]

    -- get network ID
    nid <- choice [ requireMessageField sendNetworkID
                  , throwS "sendMessage" "Missing network ID"
                  ]

    -- message parameters
    par <- messageField sendParams

    -- message content
    cnt <- messageField sendContent

    -- build chat message
    let msg = emptyChatMessage &~ do
                case ty of
                  Left  t -> messageType      .= Just t
                  Right t -> messageTypeOther .= Just t
                messageParams  .= par
                messageContent .= cnt

    let nid' = NetworkID $ fromIntegral nid

    withAccount $ \aid ->
      Event.sendMessage aid nid' msg

    return responseOkMessage
