{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Irc2me.Events.ClientMessageEvent where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
--import Control.Monad.Maybe
import Control.Monad.Reader

--text
import qualified Data.Text as Text

-- lens
import Control.Lens

-- local
import Control.Concurrent.Event

import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks
import Irc2me.Events.Types
import Irc2me.Events.Helper
import Irc2me.Frontend.Connection.Types
import Irc2me.Frontend.Messages

clientMessageEvent
  :: (MonadIO m, Functor m, MonadReader AccountState m, MonadEventW m AccountEvent)
  => AccountID
  -> ClientConnection
  -> ClientMessage
  -> m Bool
clientMessageEvent aid cc clm = choice

  [ do  -- SEND message
        (nid, cm) <- require $ sendChatMessage clm
        success <- sendIrcMessage nid cm


        if success then do

          -- resend message with current identity as 'from user'
          musr <- ircNetworkUser nid
          let cm' = cm & messageFromUser .~ musr
          raiseEvent $ AccountEvent aid (ChatMessageEvent nid cm')

          sendResponse responseOkMessage
         else
          sendResponse $ responseErrorMessage (Just "Invalid SEND message")
  ]
 where
  sendResponse = liftIO . (cc ^. ccSend)

sendChatMessage
  :: ClientMessage
  -> Maybe (NetworkID, ChatMessage)
sendChatMessage clm = do

  -- require 'send' field
  send <- clm ^. clSendMessage

  -- figure out type
  ty <- (Left <$> send ^. sendType)
        <|>
        (Right <$> send ^. sendTypeOther)

  let params  = send ^. sendParams
      content = send ^. sendContent

  -- some simple validation: quit on empty input in private messages
  when (ty == Left PRIVMSG && Text.null (content ^. non "")) Nothing

  -- network ID
  nid <- NetworkID . fromIntegral <$> send ^. sendNetworkID

  return . (nid,) $ emptyChatMessage &~ do
    case ty of
      Left  t -> messageType      .= Just t
      Right o -> messageTypeOther .= Just o
    messageParams  .= params
    messageContent .= content
