{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module for server to client messages
module ProtoBuf.Server where

import Data.ProtocolBuffers
import Data.TypeLevel.Num

import GHC.Generics (Generic)

import ProtoBuf.Instances ()
import ProtoBuf.Server.IRC
import ProtoBuf.Network

data ServerMsgType
  = SrvMsg_Response
  | SrvMsg_IRC
  | SrvMsg_Network
  deriving (Eq, Show, Enum)

data PB_ServerMessage = PB_ServerMessage
  { server_msg_type   :: Required D1  (Enumeration ServerMsgType)
    -- response messages
  , response_msg      :: Optional D10 (Message PB_Response)
    -- networks
  , network_msg       :: Repeated D20 (Message PB_Network)
    -- IRC messages
  , irc_msg           :: Optional D30 (Message PB_IrcMessage)
  }
  deriving (Show, Generic)

instance Encode PB_ServerMessage
instance Decode PB_ServerMessage

--------------------------------------------------------------------------------
-- Responses

data ResponseCode
  = ResponseOK
  | ResponseError
  deriving (Eq, Enum, Show)

data PB_Response = PB_Response
  { rsp_code        :: Optional D10 (Enumeration ResponseCode)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Response
instance Decode PB_Response
