{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Events.ChatMessageEvent where

import Control.Monad
import Control.Monad.State
import Control.Monad.Maybe

import Data.Maybe

import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Map as Map

-- lens
import Control.Lens hiding (Identity)

-- local
import Irc2me.Database.Tables.Networks
import Irc2me.Frontend.Messages as Msg
import Irc2me.Events.Types
-- import Irc2me.Events.Helper

buildChatMessageResponse
  :: (MonadIO m, MonadState AccountState m)
  => NetworkID
  -> ChatMessage
  -> m (Maybe ServerMessage)
buildChatMessageResponse nid@(NetworkID nid') cm = runMaybeT $ do

  mident <- preuse $ connectedIrcNetworks . at nid . _Just . ircIdentity
  let mnick = mident ^. _Just . identityNick

  -- see if user nick of `cm` is the current identity nick name
  let unick    = cm ^? messageFromUser . _Just . userNick
      fromSelf = isJust mnick && mnick == unick

  -- see if current user is recipient of `cm`
  let isRecipient = (mnick ^. non "") `elem` params

  let ircState = connectedIrcNetworks . at nid . _Just

  {-
   - Analyze message
   -
   -}

  case cm ^. messageType of

    -- handle JOIN events
    Just JOIN

      | fromSelf, [chan] <- params -> do

        -- keep track of active channels
        ircState . ircChannels %= Set.insert chan

        return $ sendToChannels [chan]

      | not fromSelf
      , Just nick <- unick    -- nick name of user joining
      , [chan]    <- params   -- channel name
      -> do

        -- add `nick` to channel user list
        let f = Just . maybe (Set.singleton nick) (Set.insert nick)
        ircState . ircUsers %= Map.alter f chan

        return $ sendToChannels [chan]

    -- handle PART events
    Just PART

      | fromSelf, [chan] <- params -> do

        -- remove channel from current list of channels
        ircState . ircChannels %= Set.delete chan

        -- remove user list of channel
        ircState . ircUsers %= Map.delete chan

        return $ sendToChannels [chan]

      | not fromSelf
      , Just nick <- unick    -- nick name of user joining
      , [chan]    <- params   -- channel name
      -> do

        -- remove user from user list
        ircState . ircUsers %= Map.alter (fmap $ Set.delete nick) chan

        return $ sendToChannels [chan]

    -- handle NAMES list
    Nothing
      | isNamesList
      , [_, _, chan] <- params
      -> do

        -- insert all users
        let userSet = Set.fromList $ Text.words $ content ^. non ""
            f       = Just . maybe userSet (Set.union userSet)
        ircState . ircUsers %= Map.alter f chan

        -- send nothing until end of names list is reached
        sendNothing

      | isEndOfNamesList
      , [_, chan] <- params
      -> do

        -- look up names of current channel
        ul <- use $ connectedIrcNetworks . at nid . _Just
                  . ircUsers . at chan . non' _Empty

        -- build user list message
        let msg = serverMessage $ network & networkChannels .~ [ emptyChannel &~ do
                    channelName  .= Just chan
                    channelUsers .= map (\nick -> emptyUser & userNick .~ nick)
                                        (Set.toList ul)
                    ]
        return msg

    -- other messages
    _ | isRecipient -> return sendFromNetwork -- private/network message
      | otherwise   -> return $ sendToChannels params

 where

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

  sendFromNetwork =
    serverMessage $ network & networkMessages .~ [ cm ]

  sendNothing = mzero

  {-
   - Helper
   -
   -}

  serverMessage nw = emptyServerMessage & serverNetworks .~ [ nw ]

  network = emptyNetwork &~ do
    Msg.networkId .= Just (fromIntegral nid')

  otherType = cm ^. messageTypeOther
  params = cm ^. messageParams
  content = cm ^. messageContent

  -- other message types

  isNamesList      = otherType == Just "353"
  isEndOfNamesList = otherType == Just "366"
