{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Events.ChatMessageEvent where

import Control.Monad.State

import Data.Maybe

import qualified Data.Set as Set

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

  {-
   - Analyze message
   -
   -}

  mchannels <- case cm ^. messageType of

    -- handle JOIN events on new channels
    Just JOIN
      | fromSelf, [chan] <- params -> do
        connectedIrcNetworks . at nid . _Just . ircChannels %= Set.insert chan
        return $ Just [chan]

    -- handle PART events
    Just PART
      | fromSelf, [chan] <- params -> do
        connectedIrcNetworks . at nid . _Just . ircChannels %= Set.delete chan
        return $ Just [chan]

    _ | isRecipient -> return Nothing -- private/network message
      | otherwise   -> return $ Just (cm ^. messageParams)

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
