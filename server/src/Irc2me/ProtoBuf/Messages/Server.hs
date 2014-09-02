{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for server to client messages
module Irc2me.ProtoBuf.Messages.Server where

import Control.Lens
import Data.Text (Text)

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH

-- local
import Irc2me.ProtoBuf.Messages.IrcIdentity
import Irc2me.ProtoBuf.Messages.System

data ResponseCode
  = ResponseOK
  | ResponseError
  deriving (Eq, Enum, Show)

data ServerMessage = ServerMessage
  { _serverResponseID :: Optional 3 (Value ID_T)
  , _serverSystemMsg  :: Optional 5 (Enumeration SystemMsg)

    -- response messages
  , _responseCode     :: Optional 10 (Enumeration ResponseCode)
  , _responseMsg      :: Optional 15 (Value Text)

    -- identities
  -- , _ident_list        :: Repeated 20 (Message IrcIdentity)

    -- networks
  -- , _network_list      :: Repeated 30 (Message Network)

  }
  deriving (Show, Generic)

instance Encode ServerMessage
instance Decode ServerMessage

------------------------------------------------------------------------------
-- Lenses

makePrisms      ''ResponseCode
makeFieldLenses ''ServerMessage

------------------------------------------------------------------------------
-- Standard messages

emptyServerMessage :: ServerMessage
emptyServerMessage = ServerMessage
  { _serverResponseID = putField Nothing
  , _serverSystemMsg  = putField Nothing
  , _responseCode     = putField Nothing
  , _responseMsg      = putField Nothing
  }

responseOkMessage :: ServerMessage
responseOkMessage = emptyServerMessage &~ do
  responseCode .= Just ResponseOK

responseErrorMessage :: Maybe Text -> ServerMessage
responseErrorMessage errormsg = emptyServerMessage &~ do
  responseCode .= Just ResponseError
  responseMsg  .= errormsg
