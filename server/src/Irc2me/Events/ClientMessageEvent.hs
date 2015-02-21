{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Irc2me.Events.ClientMessageEvent where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader

-- containers
import qualified Data.Map as Map

-- time
import Data.Time

--text
import qualified Data.Text as Text

-- lens
import Control.Lens

-- hdbc
import Database.HDBC

-- local
import Control.Concurrent.Event

import Irc2me.Database.Query
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks
import Irc2me.Events.Types
import Irc2me.Events.Helper
import Irc2me.Frontend.Connection.Types
import Irc2me.Frontend.Messages                 as M
import Irc2me.Frontend.Messages.Helper (epoch)

-- constraints for the client message event monad
type MonadCM m =
  ( MonadIO m
  , Functor m
  , MonadReader AccountState m
  , MonadEventW m Event
  )

-- constraint for message handler
type MonadCMH m =
  ( MonadCM m
  , MonadPlus m
  , MonadError SqlError m
  )

clientMessageEvent
  :: MonadCM m
  => AccountID
  -> ClientConnection
  -> ClientMessage
  -> m (Either SqlError Bool)
clientMessageEvent aid cc clm = oneOf

  [ handleGetMessage  aid clm
  , handleSendMessage aid clm
  ]

 where
  sendResponse = liftIO . (cc ^. ccSend)

  oneOf =
    runExceptT . choice . map (sendResponse =<<)

{-
 - GET requests
 -
 -}

handleGetMessage
  :: MonadCMH m
  => AccountID
  -> ClientMessage
  -> m ServerMessage
handleGetMessage aid clm

  -- list all stored networks
  | Just LIST_NETWORKS <- listRequest clm = do

    networks <- runQuery $ selectNetworks aid
    return $ emptyServerMessage & serverNetworks .~ networks

  -- list all conversations
  | Just LIST_CONVERSATIONS <- listRequest clm = do

    ns <- asks (^. connectedIrcNetworks)
    let f nid is res = buildConversationMessage nid is : res
        networks     = Map.foldrWithKey f [] ns

    return $ emptyServerMessage & serverNetworks .~ networks

  | otherwise = mzero

 where

  -- build network message for all conversations of the current network
  buildConversationMessage (NetworkID nid) ircState = emptyNetwork &~ do

    -- store the network ID
    M.networkId .= Just (fromIntegral nid)

    -- store all channel names
    M.networkChannels .= [ emptyChannel & channelName .~ Just c
                         | c <- ircState ^. ircChannels . to Map.keys
                         ]

    -- store the nickname of all query users
    M.networkQueries  .= [ emptyPrivateQuery u
                         | u <- ircState ^. ircQueries . to Map.keys
                         ]

listRequest :: ClientMessage -> Maybe ListRequest
listRequest clm = clm ^? clGET   . _Just
                       . getList . _Just

{-
 - Sending messages
 -
 -}

handleSendMessage
  :: MonadCMH m
  => AccountID
  -> ClientMessage
  -> m ServerMessage
handleSendMessage aid clm = do
  (nid, mu, cm) <- require $ sendChatMessage clm
  success <- sendIrcMessage nid cm

  if success then do

    let msgTy = cm ^. messageType

    -- 'rebroadcast' private messages
    when (msgTy == Just PRIVMSG) $ do

      -- current time
      now <- liftIO getCurrentTime

      -- current identity
      musr <- ircNetworkUser nid

      -- resend message with fixed 'from user' and 'timestamp'
      let cm' = cm & messageFromUser  .~ musr
                   & messageTimestamp .~ Just (now ^. epoch)
      raiseEvent $ AccountEvent aid (ChatMessageEvent nid mu cm')

    return responseOkMessage
   else
    return $ responseErrorMessage (Just "Invalid SEND message")

sendChatMessage
  :: ClientMessage
  -> Maybe (NetworkID, Maybe User, ChatMessage)
sendChatMessage clm = do

  -- require 'send' field
  send <- clm ^. clSendMessage

  -- figure out type
  ty <- (Left <$> send ^. sendType)
        <|>
        (Right <$> send ^. sendTypeOther)

  -- figure out recipient
  let musr | ty == Left PRIVMSG = send ^. sendToUser
           | otherwise          = Nothing

  let params
        | Just usr <- musr = [ usr ^. userNick ]
        | otherwise        = send ^. sendParams

  -- content
  let content = send ^. sendContent

  -- some simple validation: quit on empty input in private messages
  when (ty == Left PRIVMSG && Text.null (content ^. non "")) Nothing

  -- network ID
  nid <- NetworkID . fromIntegral <$> send ^. sendNetworkID

  return . (nid,musr,) $ emptyChatMessage &~ do
    case ty of
      Left  t -> messageType      .= Just t
      Right o -> messageTypeOther .= Just o
    messageParams  .= params
    messageContent .= content
