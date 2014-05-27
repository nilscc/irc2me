{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

-- | Module for client to server messages
module ProtoBuf.Client where

import Data.ProtocolBuffers
import Data.Word

import GHC.Generics (Generic)

import ProtoBuf.Instances ()
import ProtoBuf.Network

data ClientMsgType
  = Request
  | AddNetwork
  deriving (Eq, Show, Enum)

data PB_ClientMessage = PB_ClientMessage
  { client_msg_type     :: Required 1  (Enumeration ClientMsgType)
    -- networks
  , addnetwork_msg      :: Repeated 10 (Message PB_Network)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_ClientMessage
instance Decode PB_ClientMessage

--------------------------------------------------------------------------------
-- Requests

data Request
  = SetOpMode
  | GetBacklog
  deriving (Eq, Enum, Show)

data OpMode
  = OpModeStandard
  | OpModeBackground
  deriving (Eq, Enum, Show)

data PB_Request = PB_Request
  { rq_request          :: Required 1  (Enumeration Request)
    -- mode changes
  , rq_opmode           :: Optional 10 (Enumeration OpMode)
    -- message transfer
  , rq_msg_max_backlog  :: Optional 20 (Value Word32)
  }
  deriving (Generic, Show)

instance Encode PB_Request
instance Decode PB_Request
