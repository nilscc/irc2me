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

import Data.Maybe
import Data.Foldable as Foldable

-- containers
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

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

  [ handleGetListMessage    aid clm
  , handleGetBacklogMessage aid clm
  , handleSendMessage       aid clm
  ]

 where
  sendResponse = liftIO . (cc ^. ccSend)

  oneOf =
    runExceptT . choice . map (sendResponse =<<)

{-
 - GET requests
 -
 -}

-- | Simple list requests
handleGetListMessage
  :: MonadCMH m
  => AccountID
  -> ClientMessage
  -> m ServerMessage
handleGetListMessage aid clm

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

-- | Backlog requests
handleGetBacklogMessage
  :: MonadCMH m
  => AccountID
  -> ClientMessage
  -> m ServerMessage
handleGetBacklogMessage _aid clm

  | Just bl <- backlogRequest clm = do

    let nid = bl ^. backlogRequestNetwork

    -- state helper
    let ircState    = connectedIrcNetworks . at (NetworkID $ fromIntegral nid) . _Just
        chanState c = ircState . ircChannels . at' c
        qryState  n = ircState . ircQueries  . at' u
         where
          u = emptyUser & userNick .~ n

    backlog <- asks . flip (^.) $ case () of
      _ | Just nick <- bl ^. backlogRequestQuery -> do
          qryState nick . queryBacklog
        | Just chan <- bl ^. backlogRequestChannel -> do
          chanState chan . channelBacklog
        | otherwise -> do
          ircState . ircNetworkBacklog

    let filteredBacklog = toList $ limitBacklog bl backlog

    -- build response message
    return $ serverMsg $ emptyNetwork &~ do

      M.networkId .= Just nid

      case () of

        _ | Just n <- bl ^. backlogRequestQuery -> do
            let u = emptyUser & userNick .~ n
            netwQry $ emptyPrivateQuery u &~
              queryMessages .= filteredBacklog

          | Just c <- bl ^. backlogRequestChannel -> do
            netwChn $ emptyChannel &~ do
              channelName     .= Just c
              channelMessages .= filteredBacklog

          | otherwise -> do
            M.networkMessages .= filteredBacklog

  | otherwise = mzero
 where

  -- message building helper
  serverMsg netw = emptyServerMessage & serverNetworks .~ [ netw ]
  netwQry q = M.networkQueries  .= [ q ]
  netwChn c = M.networkChannels .= [ c ]

  -- take `i` elements from the right end of a sequence
  taker i s = Seq.drop (Seq.length s - i) s

  -- apply "limit", "before" and "after" filters to given backlog
  limitBacklog blreq bl
    = taker (fromIntegral $ blreq ^. backlogLimit . non 50)
    . fromMaybe id (blreq ^? backlogBefore . _Just . to filterBefore)
    . fromMaybe id (blreq ^? backlogAfter  . _Just . to filterAfter)
    $ bl
   where
    filterAfter t = Seq.dropWhileR $ \msg -> fromMaybe False $ do
      t' <- msg ^. messageTimestamp
      return $ t < t'
    filterBefore t = Seq.dropWhileR $ \msg -> fromMaybe False $ do
      t' <- msg ^. messageTimestamp
      return $ t > t'

backlogRequest :: ClientMessage -> Maybe BacklogRequest
backlogRequest clm = clm ^?
    clGET . _Just . getBacklog . _Just

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
