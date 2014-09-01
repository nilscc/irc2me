{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Irc2me.ProtoBuf.Messages.IRC where

-- import Data.ByteString (ByteString)
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH
import Data.Text          (Text)

import GHC.Generics (Generic)

-- lens package
import Control.Lens

--------------------------------------------------------------------------------
-- IRC messages

data IrcType
  = PrivateMessage
  | Notice
  | Join
  | Part
  | Quit
  | Kick
  | Nick
  | Topic
  | MessageOfTheDay
  deriving (Eq, Ord, Show, Enum)

data IrcMessage = IrcMessage
  { _ircMessageType       :: Optional 1  (Message IrcType)
  , _ircFromUser          :: Optional 10 (Message IrcUser)
  , _ircFromServer        :: Optional 11 (Value Text)
  , _ircTo                :: Repeated 15 (Value Text)
  , _ircContent           :: Optional 20 (Value Text)
  }
  deriving (Show, Generic)

data IrcUserflag = Operator | Voice
  deriving (Show, Eq, Ord, Enum)

data IrcUser = IrcUser
  { _userNick           :: Required 1 (Value Text)
  , _userName           :: Optional 2 (Value Text)
  , _userHost           :: Optional 3 (Value Text)
  , _userFlag           :: Optional 4 (Enumeration IrcUserflag)
  }
  deriving (Show, Generic)

--
-- instances, TH etc
--

makeFieldLenses ''IrcMessage
makePrisms      ''IrcType

makeFieldLenses ''IrcUser
makePrisms      ''IrcUserflag
