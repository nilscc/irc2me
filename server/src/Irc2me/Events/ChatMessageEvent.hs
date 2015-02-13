{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Events.ChatMessageEvent where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Maybe

import Data.Maybe

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import qualified Data.Map as Map

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
import Irc2me.Events.Types
-- import Irc2me.Events.Helper

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
  let isRecipient = (mnick ^. non "") `elem` params

  let ircState = connectedIrcNetworks . at nid . _Just

  {-
   - Analyze message
   -
   -}

  case cm ^. messageType of

    -- private messages
    Just PRIVMSG

      | isRecipient -> return sendPrivate

      | fromSelf
      , Just u <- mqueryuser
      -> do
        return $ sendQuery u cm

      | otherwise -> return $ sendToChannels params

    -- notifications
    Just NOTICE

      | isRecipient -> return sendPrivate
      | otherwise   -> return $ sendToChannels params

    -- topic
    Just TOPIC

      | isRecipient
      , [_, c] <- params
      -> do
        return $ sendToChannels [c]

      | [c] <- params
      -> do
        return $ sendToChannels [c]

      | otherwise -> do
        liftIO $ hPutStrLn stderr "Invalid TOPIC"
        sendNothing

    -- who set topic / what time
    Nothing
      | isTopicWho
      , isRecipient
      , [_, _c, _u, _t] <- params
      -> do
        sendNothing -- TODO: Implement TOPIC WHO

    -- invites
    Just INVITE

      | isRecipient -> return sendPrivate
      | otherwise   -> return $ sendToChannels params

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
        usrs <- use $ connectedIrcNetworks . at nid . _Just . ircUsers
        let channels = Map.keys $ Map.filter (Set.member nick) usrs

        return $ sendToChannels channels

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
                    channelUsers .= map (\nick -> emptyUser &~ do
                                          case Text.uncons nick of
                                            Just ('@', n) -> do
                                              userFlag .= Just Operator
                                              userNick .= n
                                            Just ('+', n) -> do
                                              userFlag .= Just Voice
                                              userNick .= n
                                            _ -> do
                                              userNick .= nick
                                        )
                                        (Set.toList ul)
                    ]
        return msg

    -- other messages
    _ | isRecipient -> return sendPrivate
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

  sendPrivate

    | Just u <- cm ^. messageFromUser
    = let cm' = cm & messageFromUser .~ Nothing
      in sendQuery u cm'

    | otherwise
    = serverMessage $ network & networkMessages .~ [ cm ]

  sendQuery u cm' =

    let query = emptyPrivateQuery u & queryMessages .~ [ cm' ]
    in
    serverMessage $ network & networkQueries .~ [ query ]

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
