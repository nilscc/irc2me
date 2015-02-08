{-# LANGUAGE PatternGuards #-}

module Irc2me.Events.ChatMessageEvent where

import Control.Lens hiding (Identity)

import Irc2me.Database.Tables.Networks
import Irc2me.Frontend.Messages as Msg

buildChatMessageResponse
  :: NetworkID
  -> Maybe Identity
  -> ChatMessage
  -> ServerMessage
buildChatMessageResponse (NetworkID nid) ident cm

  | Just _ty  <- cm ^. messageType             -- known type
  , [to']     <- cm ^. messageParams           -- one recipient
  , Just nick <- ident ^. _Just . identityNick -- current nickname
  , to' /= nick                                -- message to channel
  = serverMessage $ network &~ do
      networkChannels .= [ emptyChannel &~ do
        channelName .= Just to'
        channelMessages .= [ cm ]
        ]

  -- private message
  | otherwise = serverMessage $ network &~ do
      networkMessages .= [ cm ]

 where
  serverMessage nw = emptyServerMessage & serverNetworks .~ [ nw ]

  network = emptyNetwork &~ do
    Msg.networkId .= Just (fromIntegral nid)
