{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Types.Message where

import Control.Applicative
import Data.Aeson
import Data.Time
import Data.Text (Text)
import qualified Data.Text as Text

data OneOf a b c = First a | Second b | Third c
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Status messages

data StatusMessage
  = StatusOK
  | StatusFailed (Maybe Text)
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Account messages

data CreateAccount = CreateAccount
  { createAccountLogin :: Text
  , createAccountPassword :: Text
  }
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Chat messages

type Server = Text

type Channel = Text

data User = User
  { userNick :: Text
  , userName :: Maybe Text
  , userHost :: Maybe Text
  , userFlag :: Maybe UserFlag
  }
  deriving (Show, Eq, Ord)

data UserFlag = UserOperator | UserVoice
  deriving (Show, Read, Eq, Ord)

data MessageType
  = PRIVMSG
  | JOIN
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
  { messageTime       :: UTCTime
  , messageType       :: MessageType
  , messageFrom       :: OneOf Server Channel (Maybe User)
  , messageParameters :: [Text]
  , messageText       :: Maybe Text
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Aeson instances: To JSON

instance ToJSON StatusMessage where
  toJSON StatusOK               = ""
  toJSON (StatusFailed mreason) = object [ "fail" .= mreason ]

instance ToJSON User where
  toJSON (User nick name host flag) = object
    [ "nick" .= nick
    , "name" .= name
    , "host" .= host
    , "flag" .= flag
    ]

instance ToJSON UserFlag where
  toJSON UserOperator = "@"
  toJSON UserVoice    = "+"

instance ToJSON MessageType where
  toJSON ty = toJSON $ show ty

instance ToJSON ChatMessage where
  toJSON (ChatMessage time ty from par txt) = object
    [ "time"        .= time
    , "type"        .= ty
    , fromPair
    , "parameters"  .= par
    , "text"        .= txt
    ]
   where
    fromPair = case from of
      First   server -> "server"   .= server
      Second  chan   -> "channel"  .= chan
      Third   user   -> "user"     .= user

--------------------------------------------------------------------------------
-- Aeson instances: From JSON

instance FromJSON CreateAccount where
  parseJSON (Object o) =
    CreateAccount
      <$> o .: "login"
      <*> o .: "password"
  parseJSON j = fail $ "Cannot parse CreateAccount from non-object: " ++ show j

instance FromJSON User where
  parseJSON (Object o) =
    User <$> o .:  "nick"
         <*> o .:? "name"
         <*> o .:? "host"
         <*> o .:? "flag"
  parseJSON j = fail $ "Cannot parse User from non-object: " ++ show j

instance FromJSON UserFlag where
  parseJSON (String t) | [(flag,"")] <- reads (Text.unpack t)
    = return flag
  parseJSON j
    = fail $ "No read for UserFlag: " ++ show j

instance FromJSON MessageType where
  parseJSON (String t) | [(ty,"")] <- reads (Text.unpack t)
    = return ty
  parseJSON j
    = fail $ "No read for MessageType: " ++ show j

instance FromJSON ChatMessage where
  parseJSON (Object o) =
    ChatMessage <$> o .:  "time"
                <*> o .:  "type"
                <*> fromPair
                <*> o .:  "parameters"
                <*> o .:? "text"
   where
    fromPair =  (First  <$> o .:  "server")
            <|> (Second <$> o .:  "channel")
            <|> (Third  <$> o .:? "user")
  parseJSON j
    = fail $ "Cannot parse ChatMessage from non-object: " ++ show j
