{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for client to server messages
module Irc2me.Frontend.Messages.Client where

import GHC.Generics (Generic)

import Data.Int
import Data.Text

-- lens
import Control.Lens

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

-- local
import Irc2me.Frontend.Messages.Helper
-- import Irc2me.Frontend.Messages.Network
import Irc2me.Frontend.Messages.System
import Irc2me.Frontend.Messages.ChatMessage as ChatMessage

data ClientMessage = ClientMessage

    -- system
  { _clResponseID       :: Optional 3  (Value ID_T)
  , _clSystemMsg        :: Optional 5  (Enumeration SystemMsg)

    -- APIs
  , _clGET              :: Optional 10 (Message GET)
  , _clSendMessage      :: Optional 20 (Message SendMessage)
  , _clPUT              :: Optional 30 (Message PUT)
  , _clDELETE           :: Optional 40 (Message DELETE)
  , _clUPDATE           :: Optional 50 (Message UPDATE)

  }
  deriving (Eq, Show, Generic)

instance Encode ClientMessage
instance Decode ClientMessage

------------------------------------------------------------------------------
-- GET API

data GET = GET
  { _getList                :: Optional 10 (Enumeration ListRequest)
  , _getBacklog             :: Optional 20 (Message BacklogRequest)
  }
  deriving (Eq, Show, Generic)

instance Encode GET
instance Decode GET

data ListRequest
  = LIST_NETWORKS
  | LIST_CONVERSATIONS
  deriving (Eq, Ord, Read, Show, Enum)

type EpochTimestamp = Int64
type ChannelName    = Text
type NickName       = Text

data BacklogRequest = BacklogRequest
  { _backlogBefore          :: Optional 1  (Value EpochTimestamp)
  , _backlogAfter           :: Optional 2  (Value EpochTimestamp)
  , _backlogLimit           :: Optional 3  (Value Int32)

  , _backlogRequestNetwork  :: Required 10 (Value ID_T)
  , _backlogRequestQuery    :: Optional 20 (Value NickName)
  , _backlogRequestChannel  :: Optional 30 (Value ChannelName)
  }
  deriving (Eq, Show, Generic)

instance Encode BacklogRequest
instance Decode BacklogRequest

------------------------------------------------------------------------------
-- SEND API

data SendMessage = SendMessage
  { _sendType            :: Optional 1  (Enumeration ChatMessage.Type)
  , _sendTypeOther       :: Optional 2  (Value Text)

  , _sendNetworkID       :: Optional 3  (Value ID_T)

  , _sendToUser          :: Optional 5  (Message User)
  , _sendParams          :: Repeated 15 (Value Text)
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
makeFieldLenses ''BacklogRequest
makePrisms      ''ListRequest

makeFieldLenses ''SendMessage

makeFieldLenses ''PUT

makeFieldLenses ''DELETE

makeFieldLenses ''UPDATE
