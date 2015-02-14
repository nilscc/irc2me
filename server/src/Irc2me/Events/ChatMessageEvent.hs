{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Events.ChatMessageEvent where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Maybe

import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified Data.Set       as Set
--import qualified Data.Sequence  as Seq
import qualified Data.Map       as Map

import System.IO

-- time
import Data.Time.Format
import System.Locale

-- lens
import Control.Lens hiding (Identity)

-- local
import Irc2me.Database.Tables.Networks
import Irc2me.Frontend.Messages        as Msg
-- import Irc2me.Frontend.Messages.Helper as Msg
import Irc2me.Events.Types             as E
import Irc2me.Events.Helper

buildChatMessageResponse
  :: (MonadIO m, MonadState AccountState m)
  => NetworkID
  -> Maybe User
  -> ChatMessage
  -> m (Maybe ServerMessage)
buildChatMessageResponse nid@(NetworkID nid') mqueryuser cm = runMaybeT $ do

  printMessage cm

  mident <- preuse $ connectedIrcNetworks . at nid . _Just . ircIdentity
  let mnick = mident ^. _Just . identityNick

  -- see if user nick of `cm` is the current identity nick name
  let unick    = cm ^? messageFromUser . _Just . userNick
      fromSelf = isJust mnick && mnick == unick

  -- see if current user is recipient of `cm`
  let isRecipient     = (mnick ^. non "") `elem` params
      isServerMessage = isJust $ cm ^. messageFromServer

  {-
   - Analyze message
   -
   -}

  case cm ^. messageType of

    -- private messages
    Just PRIVMSG

      -- receiving private query message
      | isRecipient
      , Just u <- cm ^. messageFromUser
      -> do

        -- save bandwidth by removing the "duplicate" user
        let cm' = cm & messageFromUser .~ Nothing

        -- add message to query backlog
        storeQueryBacklog u cm'

        return $ sendQuery u cm'

      -- sending private query message
      | fromSelf
      , Just u <- mqueryuser
      -> do

        -- add message to query backlog
        storeQueryBacklog u cm

        return $ sendQuery u cm

      -- message from network
      | isRecipient && isServerMessage
      -> do

        -- add message to network backlog
        storeNetworkBacklog cm

        return sendNetworkMessage

      -- public channel message
      | otherwise -> do

        -- add message to channel backlog
        case params of
          [chan] -> storeChannelBacklog chan cm
          _      -> warnMissingBacklog

        return $ sendToChannels params

    -- notifications
    Just NOTICE

      | isServerMessage
      -> do

        storeNetworkBacklog cm
        return sendPrivate

      | otherwise -> do

        -- add message to channel backlog
        case params of
          [chan] -> storeChannelBacklog chan cm
          _      -> warnMissingBacklog

        return $ sendToChannels params

    -- topic
    Just TOPIC -> do

      let mchan =
            case params of
              [_, c] | isRecipient -> Just c
              [c]                  -> Just c
              _                    -> Nothing

      let err = do
            warnMissingBacklog
            sendNothing

      maybe err `flip` mchan $ \chan -> do

        -- set channel topic
        chanState chan . channelTopic .= cm ^. messageContent

        storeChannelBacklog chan cm
        return $ sendToChannels [chan]

    -- who set topic / what time
    Nothing
      | isTopicWho
      , isRecipient
      , [_, _c, _u, _t] <- params
      -> do
        sendNothing -- TODO: Implement TOPIC WHO

    -- invites
    Just INVITE

      -- invite as private query message
      | isRecipient
      , Just u <- cm ^. messageFromUser
      -> do

        let cm' = cm & messageFromUser .~ Nothing
        storeQueryBacklog u cm'
        return $ sendQuery u cm'

      | otherwise -> do

        case params of
          [chan] -> storeChannelBacklog chan cm
          _      -> warnMissingBacklog

        return $ sendToChannels params

    -- handle JOIN events
    Just JOIN

      | fromSelf
      , [chan] <- params -> do

        -- keep track of active channels
        chanState chan . E.channelStatus .= ONLINE

        storeChannelBacklog chan cm

        return $ sendToChannels [chan]

      | not fromSelf
      , Just nick <- unick    -- nick name of user joining
      , [chan]    <- params   -- channel name
      -> do

        -- add `nick` to channel user list
        chanState chan . E.channelUsers %= Set.insert nick

        storeChannelBacklog chan cm

        return $ sendToChannels [chan]

    -- handle PART events
    Just PART

      | fromSelf, [chan] <- params -> do

        -- remove channel from current list of channels
        ircState . ircChannels %= Map.delete chan

        storeChannelBacklog chan cm

        return $ sendToChannels [chan]

      | not fromSelf
      , Just nick <- unick    -- nick name of user joining
      , [chan]    <- params   -- channel name
      -> do

        -- remove user from user list
        chanState chan . E.channelUsers %= Set.delete nick

        storeChannelBacklog chan cm

        return $ sendToChannels [chan]

    -- handle QUIT events
    Just QUIT

      | fromSelf
      -> do

        liftIO $ hPutStrLn stderr "Not implemented: Own QUIT message" -- TODO
        sendNothing

      | not fromSelf
      , Just nick <- unick
      -> do

        -- figure out all channels in which user was
        chans <- use $ connectedIrcNetworks . at nid . _Just . ircChannels
        let channels = Map.keys $
              Map.filter (^. E.channelUsers . to (Set.member nick))
                         chans

        mapM_ (storeChannelBacklog `flip` cm) channels

        return $ sendToChannels channels

    -- handle NAMES list
    Nothing

      | isNamesList
      , [_, _, chan] <- params
      -> do

        -- insert all users
        let userSet = Set.fromList $ Text.words $ content ^. non ""
        chanState chan . E.channelUsers %= Set.union userSet

        -- send nothing until end of names list is reached
        sendNothing

      | isEndOfNamesList
      , [_, chan] <- params
      -> do

        -- look up names of current channel
        ul <- use $ connectedIrcNetworks . at nid . _Just
                  . ircChannels . at chan . non' _Empty
                  . E.channelUsers

        -- build user list message
        return $
          serverMessage $ network & networkChannels .~ [ emptyChannel &~ do
            channelName      .= Just chan
            Msg.channelUsers .= [ emptyUser & userNick .~ u | u <- Set.toList ul ]
            ]

    -- other messages
    _ | isServerMessage -> do
        storeNetworkBacklog cm
        return sendNetworkMessage

      | isRecipient
      , Just u <- cm ^. messageFromUser
      -> do
        let cm' = cm & messageFromUser .~ Nothing
        storeQueryBacklog u cm'
        return $ sendQuery u cm'

      | otherwise -> do

        case params of
          [chan] -> storeChannelBacklog chan cm
          _      -> warnMissingBacklog

        return $ sendToChannels params

 where

  seqAppend = flip (|>)

  {-
   - Build server message
   -
   -}

  sendToChannels channels =
    let netchans = flip map channels $ \channel -> emptyChannel &~ do
          channelName     .= Just channel
          channelMessages .= [ cm ]
    in
    serverMessage $ network & networkChannels .~ netchans

  sendPrivate

    | Just u <- cm ^. messageFromUser
    = let cm' = cm & messageFromUser .~ Nothing
      in sendQuery u cm'

    | otherwise = sendNetworkMessage

  sendNetworkMessage =
    serverMessage $ network & networkMessages .~ [ cm ]

  sendQuery u cm' =

    let query = emptyPrivateQuery u & queryMessages .~ [ cm' ]
    in
    serverMessage $ network & networkQueries .~ [ query ]

  sendNothing = mzero

  {-
   - Helper
   -
   -}

  otherType = cm ^. messageTypeOther
  params    = cm ^. messageParams
  content   = cm ^. messageContent

  -- building response message

  serverMessage nw = emptyServerMessage & serverNetworks .~ [ nw ]

  network = emptyNetwork &~ do
    Msg.networkId .= Just (fromIntegral nid')

  -- state helper

  ircState    = connectedIrcNetworks . at nid . _Just
  chanState c = ircState . ircChannels . at' c
  qryState  u = ircState . ircQueries . at' (u ^. userNick)

  -- backlog functions

  storeNetworkBacklog cm' = do
    ircState . ircNetworkBacklog %= seqAppend cm'

  storeChannelBacklog chan cm' = do
    chanState chan . channelBacklog %= seqAppend cm'

  storeQueryBacklog usr cm' = do
    qryState usr . queryBacklog %= seqAppend cm'

  warnMissingBacklog = liftIO $ hPutStrLn stderr $
    "Warning: No backlog available for "
    ++ ty ++ " (message parameter: "
    ++ Text.unpack (Text.intercalate " " params)
    ++ ")"
   where
    ty | Just t <- cm ^. messageType      = show t
       | Just o <- cm ^. messageTypeOther = Text.unpack o
       | otherwise = "?"

  -- other message types

  isNamesList      = otherType == Just "353"
  isEndOfNamesList = otherType == Just "366"
  isTopicWho       = otherType == Just "333"

-- Pretty printing chat messages
printMessage :: MonadIO m => ChatMessage -> m ()
printMessage cm = liftIO . Text.putStrLn
  . Text.intercalate " "
  . catMaybes
  $ [ par "[" "]" <$> timestamp
    , ty
    , from'
    , par "[" "]" <$> to'
    , cont
    ]
 where
  infixr 5 .++
  (.++) = Text.append

  par l r t = l .++ t .++ r

  timestamp = cm ^? messageTime . to txt
   where
    txt = Text.pack . formatTime defaultTimeLocale "%R"

  ty
    | Just t <- cm ^. messageType
    = Just . Text.pack $ show t
    | Just t <- cm ^. messageTypeOther
    = Just . Text.pack $ show t
    | otherwise = Nothing

  from' =  (cm ^? messageFromUser . _Just . userNick . to (par "<" ">"))
       <|> (cm ^? messageFromServer . _Just          . to (par "(" ")"))

  to' = Just $ Text.intercalate ", " (cm ^. messageParams)

  cont = cm ^. messageContent
