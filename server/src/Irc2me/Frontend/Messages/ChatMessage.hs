{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Irc2me.Frontend.Messages.ChatMessage where

import Control.Applicative
import Control.Monad

import Data.Int
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
import Irc2me.Frontend.Messages.Helper

--------------------------------------------------------------------------------
-- IRC messages

data Type
  = JOIN
  | PART
  | INVITE
  | QUIT
  | KICK
  | NICK
  | NOTICE
  | TOPIC
  | MOTD
  deriving (Eq, Ord, Show, Read, Enum)

data ChatMessage = ChatMessage
  { -- type: one of
    _messageType          :: Optional 1  (Enumeration Type)
  , _messageTypeOther     :: Optional 2  (Value Text)

  , _messageTimestamp     :: Optional 5  (Value Int64)
  , _messageFromUser      :: Optional 10 (Message User)
  , _messageFromServer    :: Optional 11 (Value Text)

  , _messageParams        :: Repeated 15 (Value Text)
  , _messageContent       :: Optional 20 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode ChatMessage
instance Decode ChatMessage

data Userflag = Operator | Voice
  deriving (Show, Eq, Ord, Enum)

data User = User
  { _userNick           :: Required 1 (Value Text)
  , _userName           :: Optional 2 (Value Text)
  , _userHost           :: Optional 3 (Value Text)
  , _userFlag           :: Optional 4 (Enumeration Userflag)
  }
  deriving (Eq, Show, Generic)

instance Encode User
instance Decode User

--------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''ChatMessage
makePrisms      ''Type

makeFieldLenses ''User
makePrisms      ''Userflag

--
-- IRC user isomorphism
--

user :: Iso' IRC.UserInfo User
user = iso toUser fromUser

emptyUser :: User
emptyUser = User (putField "") mempty mempty mempty

fromUser :: User -> IRC.UserInfo
fromUser usr = IRC.UserInfo
  (addFlag $ usr ^. userNick . from encoded)
  (          usr ^? userName . _Just . from encoded)
  (          usr ^? userHost . _Just . from encoded)
 where
  addFlag bs = maybe bs (\f -> B8.cons (flag f) bs)
                     (usr ^. userFlag)
  flag Operator = '@'
  flag Voice    = '+'

toUser :: IRC.UserInfo -> User
toUser ui = emptyUser &~ do
  userNick .= IRC.userNick ui ^. encoded
  userName .= IRC.userName ui ^? _Just . encoded
  userHost .= IRC.userHost ui ^? _Just . encoded

--
-- IRC message isomorphism
--

type Parameters = [Text]

chatMessage :: Iso' IRC.IRCMsg (ChatMessage, Parameters)
chatMessage = iso toChatMsg fromChatMsg

emptyChatMessage :: ChatMessage
emptyChatMessage = ChatMessage
  { _messageType        = putField Nothing
  , _messageTypeOther   = putField Nothing

  , _messageTimestamp   = putField Nothing
  , _messageFromUser    = putField Nothing
  , _messageFromServer  = putField Nothing

  , _messageParams      = putField []
  , _messageContent     = putField Nothing
  }

toChatMsg :: IRC.IRCMsg -> (ChatMessage, Parameters)
toChatMsg msg = (,to') $ emptyChatMessage &~ do

  when (IRC.msgCmd msg /= "PRIVMSG") $
    case IRC.msgCmd msg ^? msgType of
      Just ty        -> messageType      .= Just ty
      Nothing        -> messageTypeOther .= Just (IRC.msgCmd msg ^. encoded)

  messageFromUser     .= fromU
  messageFromServer   .= fromS
  messageContent      .= if T.null content then Nothing else Just content

 where

  -- user / server info
  fromU = case IRC.msgPrefix msg of
    Just (Left ui) -> Just (ui ^. user)
    _              -> Nothing
  fromS = case IRC.msgPrefix msg of
    Just (Right s) -> Just (s ^. encoded)
    _              -> Nothing

  -- to?
  to' = map (^. encoded) (IRC.msgParams msg)

  -- content
  content = IRC.msgTrail msg ^. encoded

fromChatMsg :: (ChatMessage, Parameters) -> IRC.IRCMsg
fromChatMsg (msg, to') = IRC.IRCMsg
  { IRC.msgPrefix = prefix
  , IRC.msgCmd    = fromMaybe "PRIVMSG" cmd
  , IRC.msgParams = map (^. from encoded) to'
  , IRC.msgTrail  = fromMaybe "" content
  }
 where

  prefix  =  (Left  <$> msg ^? messageFromUser   . _Just . from user)
         <|> (Right <$> msg ^? messageFromServer . _Just . from encoded)

  cmd     =  msg ^? messageType      . _Just . re msgType
         <|> msg ^? messageTypeOther . _Just . from encoded

  content = msg ^? messageContent . _Just . from encoded

--
-- IRC type prism
--

msgType :: Prism' ByteString Type
msgType = prism' fromType toType

fromType :: Type -> ByteString
fromType cmd = case cmd of
  NOTICE -> "NOTICE"
  JOIN   -> "JOIN"
  PART   -> "PART"
  INVITE -> "INVITE"
  QUIT   -> "QUIT"
  KICK   -> "KICK"
  NICK   -> "NICK"
  TOPIC  -> rpl_TOPIC
  MOTD   -> "MOTD"

toType :: ByteString -> Maybe Type
toType bs = case bs of
  "NOTICE"          -> Just NOTICE
  "JOIN"            -> Just JOIN
  "PART"            -> Just PART
  "INVITE"          -> Just INVITE
  "QUIT"            -> Just QUIT
  "KICK"            -> Just KICK
  "NICK"            -> Just NICK
  _ | is rpl_TOPIC  -> Just TOPIC
    | otherwise     -> Nothing
 where
  is cmd = cmd == bs
