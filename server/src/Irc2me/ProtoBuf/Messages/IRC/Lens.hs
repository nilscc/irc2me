{-# LANGUAGE OverloadedStrings #-}

module Irc2me.ProtoBuf.Messages.IRC.Lens where

import Control.Applicative

import Data.Maybe
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T

-- lens package
import Control.Lens

-- protobuf package
import Data.ProtocolBuffers

-- irc-bytestring package
import qualified Network.IRC.ByteString.Parser as IRC

import Network.IRC.Message.Codes
import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages.IRC

--
-- IRC user iso
--

ircUser :: Iso' IRC.UserInfo IrcUser
ircUser = iso toIrcUser fromIrcUser

emptyIrcUser :: IrcUser
emptyIrcUser = IrcUser (putField "") mempty mempty mempty

fromIrcUser :: IrcUser -> IRC.UserInfo
fromIrcUser usr = IRC.UserInfo
  (addFlag $ usr ^. userNick . from encoded)
  (          usr ^? userName . _Just . from encoded)
  (          usr ^? userHost . _Just . from encoded)
 where
  addFlag bs = maybe bs (\f -> B8.cons (flag f) bs)
                     (usr ^. userFlag)
  flag Operator = '@'
  flag Voice    = '+'

toIrcUser :: IRC.UserInfo -> IrcUser
toIrcUser ui = emptyIrcUser &~ do
  userNick .= IRC.userNick ui ^. encoded
  userName .= IRC.userName ui ^? _Just . encoded
  userHost .= IRC.userHost ui ^? _Just . encoded

--
-- IRC message prism
--

ircMessage :: Prism' IRC.IRCMsg IrcMessage
ircMessage = prism' fromIrcMsg toIrcMsg

emptyIrcMessage :: IrcMessage
emptyIrcMessage = IrcMessage
  { _ircMessageType = putField Nothing
  , _ircFromUser    = putField Nothing
  , _ircFromServer  = putField Nothing
  , _ircTo          = putField []
  , _ircContent     = putField Nothing
  }

toIrcMsg :: IRC.IRCMsg -> Maybe IrcMessage
toIrcMsg msg

  | Just ty <- IRC.msgCmd msg ^? ircType
  = Just $ emptyIrcMessage &~ do
      ircMessageType  .= Just ty
      ircFromUser     .= fromU
      ircFromServer   .= fromS
      ircTo           .= to'
      ircContent      .= if T.null content then Nothing else Just content

  | otherwise
  = Nothing

 where

  -- user / server info
  fromU = case IRC.msgPrefix msg of
    Just (Left ui) -> Just (ui ^. ircUser)
    _              -> Nothing
  fromS = case IRC.msgPrefix msg of
    Just (Right s) -> Just (s ^. encoded)
    _              -> Nothing

  -- to?
  to' = map (^. encoded) (IRC.msgParams msg)

  -- content
  content = IRC.msgTrail msg ^. encoded

fromIrcMsg :: IrcMessage -> IRC.IRCMsg
fromIrcMsg msg = IRC.IRCMsg
  { IRC.msgPrefix = prefix
  , IRC.msgCmd    = fromMaybe "" cmd
  , IRC.msgParams = to'
  , IRC.msgTrail  = fromMaybe "" content
  }
 where
  prefix  =  (Left  <$> msg ^? ircFromUser   . _Just . from ircUser)
         <|> (Right <$> msg ^? ircFromServer . _Just . from encoded)
  cmd     = msg ^? ircMessageType . _Just . re ircType
  to'     = map (^. from encoded) (msg ^. ircTo)
  content = msg ^? ircContent . _Just . from encoded

--
-- IRC type prism
--

ircType :: Prism' ByteString IrcType
ircType = prism' fromIrcType toIrcType

fromIrcType :: IrcType -> ByteString
fromIrcType cmd = case cmd of
  PrivateMessage  -> "PRIVMSG"
  Notice          -> "NOTICE"
  Join            -> "JOIN"
  Part            -> "PART"
  Quit            -> "QUIT"
  Kick            -> "KICK"
  Nick            -> "NICK"
  Topic           -> rpl_TOPIC
  MessageOfTheDay -> "MOTD"

toIrcType :: ByteString -> Maybe IrcType
toIrcType bs = case bs of
  "PRIVMSG"         -> Just PrivateMessage
  "NOTICE"          -> Just Notice
  "JOIN"            -> Just Join
  "PART"            -> Just Part
  "QUIT"            -> Just Quit
  "KICK"            -> Just Kick
  "NICK"            -> Just Nick
  _ | is rpl_TOPIC  -> Just Topic
    | otherwise     -> Nothing
 where
  is cmd = cmd == bs
