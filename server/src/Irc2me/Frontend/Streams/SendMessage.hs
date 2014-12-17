{-# LANGUAGE OverloadedStrings #-}

module Irc2me.Frontend.Streams.SendMessage where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.Text as Text

-- lens
import Control.Lens

-- local
import Irc2me.Events as Event

import Irc2me.Database.Tables.Networks

import Irc2me.Frontend.Streams.Helper
import Irc2me.Frontend.Streams.StreamT

import Irc2me.Frontend.Messages.ChatMessage
import Irc2me.Frontend.Messages.Client
import Irc2me.Frontend.Messages.Server

sendMessageResponse :: ServerResponse
sendMessageResponse = do

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

    -- some simple validation: empty input on private messages
    when (ty == Left PRIVMSG && Text.all isSpace (cnt ^. non "")) $
      throwS "sendMessageResponse" "Empty PRIVMSG"

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
