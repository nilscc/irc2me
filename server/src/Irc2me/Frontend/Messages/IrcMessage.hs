{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.Frontend.Messages.IrcMessage where

import Control.Applicative

import Data.Maybe
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH

-- lens package
import Control.Lens

-- protobuf package
import Data.ProtocolBuffers.Orphans ()

-- irc-bytestring package
import qualified Network.IRC.ByteString.Parser as IRC

-- local
import Network.IRC.Message.Codes
import Irc2me.Frontend.Helper

--------------------------------------------------------------------------------
-- IRC messages

data IrcType
  = PrivateMessage
  | Join
  | Part
  | Invite
  | Quit
  | Kick
  | Nick
  | Notice
  | Topic
  | MessageOfTheDay
  deriving (Eq, Ord, Show, Read, Enum)

data IrcMessage = IrcMessage
  { _ircMessageType       :: Optional 1  (Enumeration IrcType)
  , _ircMessageTypeRaw    :: Optional 2  (Value Text)

  , _ircFromUser          :: Optional 10 (Message IrcUser)
  , _ircFromServer        :: Optional 11 (Value Text)
  , _ircTo                :: Repeated 15 (Value Text)

  , _ircContent           :: Optional 20 (Value Text)
  }
  deriving (Show, Generic)

instance Encode IrcMessage
instance Decode IrcMessage

data IrcUserflag = Operator | Voice
  deriving (Show, Eq, Ord, Enum)

data IrcUser = IrcUser
  { _userNick           :: Required 1 (Value Text)
  , _userName           :: Optional 2 (Value Text)
  , _userHost           :: Optional 3 (Value Text)
  , _userFlag           :: Optional 4 (Enumeration IrcUserflag)
  }
  deriving (Show, Generic)

instance Encode IrcUser
instance Decode IrcUser

--------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''IrcMessage
makePrisms      ''IrcType

makeFieldLenses ''IrcUser
makePrisms      ''IrcUserflag

--
-- IRC user isomorphism
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
-- IRC message isomorphism
--

ircMessage :: Iso' IRC.IRCMsg IrcMessage
ircMessage = iso toIrcMsg fromIrcMsg

emptyIrcMessage :: IrcMessage
emptyIrcMessage = IrcMessage
  { _ircMessageType     = putField Nothing
  , _ircMessageTypeRaw  = putField Nothing
  , _ircFromUser        = putField Nothing
  , _ircFromServer      = putField Nothing
  , _ircTo              = putField []
  , _ircContent         = putField Nothing
  }

toIrcMsg :: IRC.IRCMsg -> IrcMessage
toIrcMsg msg = emptyIrcMessage &~ do

  case IRC.msgCmd msg ^? ircType of
    Just ty -> ircMessageType     .= Just ty
    Nothing -> ircMessageTypeRaw  .= Just (IRC.msgCmd msg ^. encoded)

  ircFromUser     .= fromU
  ircFromServer   .= fromS
  ircTo           .= to'
  ircContent      .= if T.null content then Nothing else Just content

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

  cmd     =  msg ^? ircMessageType    . _Just . re ircType
         <|> msg ^? ircMessageTypeRaw . _Just . from encoded

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
  Invite          -> "INVITE"
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
  "INVITE"          -> Just Invite
  "QUIT"            -> Just Quit
  "KICK"            -> Just Kick
  "NICK"            -> Just Nick
  _ | is rpl_TOPIC  -> Just Topic
    | otherwise     -> Nothing
 where
  is cmd = cmd == bs
