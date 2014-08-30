{-# LANGUAGE OverloadedStrings #-}

module ProtoBuf.Messages.IRC.Lens where

import Control.Applicative

import Data.Maybe
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T

-- lens package
import Control.Lens
import Data.ByteString.Lens

-- protobuf package
import Data.ProtocolBuffers

-- irc-bytestring package
import qualified Network.IRC.ByteString.Parser as IRC

import IRC.Codes
import ProtoBuf.Helper
import ProtoBuf.Messages.IRC

-- User iso

ircUser :: Iso' IRC.UserInfo User
ircUser = iso toIrcUser fromIrcUser

emptyIrcUser :: User
emptyIrcUser = User (putField "") mempty mempty mempty

fromIrcUser :: User -> IRC.UserInfo
fromIrcUser usr = IRC.UserInfo
  (addFlag $ usr ^. userNick . from encoded)
  (          usr ^? userName . _Just . from encoded)
  (          usr ^? userHost . _Just . from encoded)
 where
  addFlag bs = maybe bs (\f -> B8.cons f bs)
                     (usr ^? userFlag . _Just . to flag)
  flag Operator = '@'
  flag Voice    = '+'

toIrcUser :: IRC.UserInfo -> User
toIrcUser ui = emptyIrcUser &~ do
  userNick .= IRC.userNick ui ^. encoded
  userName .= IRC.userName ui ^? _Just . encoded
  userHost .= IRC.userHost ui ^? _Just . encoded


-- IrcMsgType prism

ircMessage :: Prism' IRC.IRCMsg IrcMessage
ircMessage = prism' fromIrcMsg toIrcMsg

emptyIrcMessage :: IrcMessage
emptyIrcMessage = IrcMessage
  { _ircPrivateMessage = putField Nothing
  , _ircStatusMessage  = putField Nothing
  }

emptyPrivateMessage :: PrivateMessage
emptyPrivateMessage = PrivateMessage
  { _pmFromUser = putField Nothing
  , _pmFromServer = putField Nothing
  , _pmTo = putField []
  , _pmMessage = putField Nothing
  }

emptyStatusMessage :: StatusMessage
emptyStatusMessage = StatusMessage
  { _smType = putField Notice
  , _smFromUser = putField Nothing
  , _smFromServer = putField Nothing
  , _smTo = putField []
  , _smMessage = putField Nothing
  }

toIrcMsg :: IRC.IRCMsg -> Maybe IrcMessage
toIrcMsg msg =
  case IRC.msgCmd msg of
    "PRIVMSG"           -> toPrivMsg msg
    _ | is `any` status -> toStatMsg msg
      | otherwise       -> Nothing
 where
  is code = B8.pack code == IRC.msgCmd msg
  status = [ "JOIN"
           , "PART"
           , "QUIT"
           , "KICK"
           , "NICK"
           , rpl_TOPIC
           , "MOTD"
           ]

toPrivMsg :: IRC.IRCMsg -> Maybe IrcMessage
toPrivMsg msg = Just $
  emptyIrcMessage & ircPrivateMessage .~ Just privMsg
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

  -- build the message
  privMsg = emptyPrivateMessage &~ do
    pmFromUser    .= fromU
    pmFromServer  .= fromS
    pmTo          .= to'
    pmMessage     .= if T.null content then Nothing else Just content

toStatMsg :: IRC.IRCMsg -> Maybe IrcMessage
toStatMsg msg

  | Just ty <- IRC.msgCmd msg ^? statusType
  = Just $ emptyIrcMessage & ircStatusMessage .~ Just (statMsg ty)

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

  -- build message
  statMsg cmd = emptyStatusMessage &~ do
    smType .= cmd
    smFromUser .= fromU
    smFromServer .= fromS
    smTo .= to'
    smMessage .= if T.null content then Nothing else Just content

fromIrcMsg :: IrcMessage -> IRC.IRCMsg
fromIrcMsg msg

  | Just priv <- msg ^. ircPrivateMessage
  = fromPrivMsg priv

  | Just stat <- msg ^. ircStatusMessage
  = fromStatMsg stat

  | otherwise = IRC.IRCMsg Nothing "" [] ""

fromPrivMsg :: PrivateMessage -> IRC.IRCMsg
fromPrivMsg pm = IRC.IRCMsg
  { IRC.msgPrefix = prefix
  , IRC.msgCmd    = "PRIVMSG"
  , IRC.msgParams = to'
  , IRC.msgTrail  = fromMaybe "" content
  }
 where
  prefix  =  (Left  <$> pm ^? pmFromUser   . _Just . from ircUser)
         <|> (Right <$> pm ^? pmFromServer . _Just . from encoded)
  to'     = map (^. from encoded) (pm ^. pmTo)
  content = pm ^? pmMessage . _Just . from encoded

fromStatMsg :: StatusMessage -> IRC.IRCMsg
fromStatMsg sm = IRC.IRCMsg
  { IRC.msgPrefix = prefix
  , IRC.msgCmd    = cmd
  , IRC.msgParams = to'
  , IRC.msgTrail  = fromMaybe "" content
  }
 where
  prefix  =  (Left  <$> sm ^? smFromUser   . _Just . from ircUser)
         <|> (Right <$> sm ^? smFromServer . _Just . from encoded)
  cmd     = sm ^. smType . re statusType
  to'     = map (^. from encoded) (sm ^. smTo)
  content = sm ^? smMessage . _Just . from encoded

statusType :: Prism' ByteString StatusType
statusType = prism' fromStatusType toStatusType

fromStatusType :: StatusType -> ByteString
fromStatusType cmd = case cmd of
  Notice          -> "NOTICE"
  Join            -> "JOIN"
  Part            -> "PART"
  Quit            -> "QUIT"
  Kick            -> "KICK"
  Nick            -> "NICK"
  Topic           -> rpl_TOPIC ^. packedChars
  MessageOfTheDay -> "MOTD"

toStatusType :: ByteString -> Maybe StatusType
toStatusType bs = case bs of
  "NOTICE"          -> Just Notice
  "JOIN"            -> Just Join
  "PART"            -> Just Part
  "QUIT"            -> Just Quit
  "KICK"            -> Just Kick
  "NICK"            -> Just Nick
  _ | is rpl_TOPIC  -> Just Topic
    | otherwise     -> Nothing
 where
  is cmd = cmd ^. packedChars == bs
