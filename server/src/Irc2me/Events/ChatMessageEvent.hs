{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Events.ChatMessageEvent where

import Control.Monad.State

import Data.Maybe

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
  -> m ServerMessage
buildChatMessageResponse nid@(NetworkID nid') cm = do

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

  mchannels <- case cm ^. messageType of

    -- handle JOIN events
    Just JOIN

      | fromSelf, [chan] <- params -> do

        -- keep track of active channels
        ircState . ircChannels %= Set.insert chan

        return $ Just [chan]

      | not fromSelf
      , Just nick <- unick    -- nick name of user joining
      , [chan]    <- params   -- channel name
      -> do

        -- add `nick` to channel user list
        let f = Just . maybe (Set.singleton nick) (Set.insert nick)
        ircState . ircUsers %= Map.alter f chan

        return $ Just [chan]

    -- handle PART events
    Just PART

      | fromSelf, [chan] <- params -> do

        -- remove channel from current list of channels
        ircState . ircChannels %= Set.delete chan

        -- remove user list of channel
        ircState . ircUsers %= Map.delete chan

        return $ Just [chan]

      | not fromSelf
      , Just nick <- unick    -- nick name of user joining
      , [chan]    <- params   -- channel name
      -> do

        -- remove user from user list
        ircState . ircUsers %= Map.alter (fmap $ Set.delete nick) chan

        return $ Just [chan]

    -- other messages
    _ | isRecipient -> return Nothing -- private/network message
      | otherwise   -> return $ Just params

  {-
   - Send server message to all channels (if any)
   -
   -}

  return $ case mchannels of

    Just channels ->

      let netchans = flip map channels $ \channel -> emptyChannel &~ do
            channelName     .= Just channel
            channelMessages .= [ cm ]
      in
      serverMessage $ network & networkChannels .~ netchans

    Nothing ->

      serverMessage $ network & networkMessages .~ [ cm ]

 where
  serverMessage nw = emptyServerMessage & serverNetworks .~ [ nw ]

  network = emptyNetwork &~ do
    Msg.networkId .= Just (fromIntegral nid')

  params = cm ^. messageParams
