{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for client to server messages
module Irc2me.Frontend.Messages.Client where

import GHC.Generics (Generic)

import Data.Text

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

-- local
import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages.Network
import Irc2me.Frontend.Messages.System
import Irc2me.Frontend.Messages.ChatMessage as ChatMessage

data ClientMessage = ClientMessage

    -- system
  { _clResponseID       :: Optional 3  (Value ID_T)
  , _clSystemMsg        :: Optional 5  (Enumeration SystemMsg)

    -- APIs
  , _clGET              :: Optional 10 (Message GET)
  , _clSendMessage      :: Optional 10 (Message SendMessage)
  , _clPUT              :: Optional 10 (Message PUT)
  , _clDELETE           :: Optional 10 (Message DELETE)
  , _clUPDATE           :: Optional 10 (Message UPDATE)

  }
  deriving (Eq, Show, Generic)

instance Encode ClientMessage
instance Decode ClientMessage

------------------------------------------------------------------------------
-- GET API

data GET = GET
  { _getNetwork         :: Repeated 1 (Message Network)
  }
  deriving (Eq, Show, Generic)

instance Encode GET
instance Decode GET

------------------------------------------------------------------------------
-- SEND API

data SendMessage = SendMessage
  { _sendType            :: Optional 1  (Enumeration ChatMessage.Type)
  , _sendTypeOther       :: Optional 2  (Value Text)

  , _sendNetworkID       :: Optional 3  (Value ID_T)

  , _sendParams          :: Optional 15 (Value Text)
  , _sendContent         :: Optional 20 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode SendMessage
instance Decode SendMessage

------------------------------------------------------------------------------
-- PUT API

data PUT = PUT
  {
  }
  deriving (Eq, Show, Generic)

instance Encode PUT
instance Decode PUT

------------------------------------------------------------------------------
-- DELETE API

data DELETE = DELETE
  {
  }
  deriving (Eq, Show, Generic)

instance Encode DELETE
instance Decode DELETE

------------------------------------------------------------------------------
-- UPDATE API

data UPDATE = UPDATE
  {
  }
  deriving (Eq, Show, Generic)

instance Encode UPDATE
instance Decode UPDATE

------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''ClientMessage

makeFieldLenses ''GET

makeFieldLenses ''SendMessage

makeFieldLenses ''PUT

makeFieldLenses ''DELETE

makeFieldLenses ''UPDATE
