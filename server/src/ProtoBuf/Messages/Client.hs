{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

-- | Module for client to server messages
module ProtoBuf.Messages.Client where

import Data.ProtocolBuffers
import Data.Text
import Data.Word

import GHC.Generics (Generic)

import ProtoBuf.Instances ()
import ProtoBuf.Messages.Identity
import ProtoBuf.Messages.Network

data PB_ClientMessage = PB_ClientMessage
  { -- acount
    auth_login          :: Optional 1 (Value Text)
  , auth_password       :: Optional 2 (Value Text)

    -- identities
  , identity_add        :: Repeated 11  (Message PB_Identity)
  , identity_remove     :: Repeated 12  (Message PB_Identity)

    -- networks
  , network_add         :: Repeated 101 (Message PB_Network)
  , network_remove      :: Repeated 102 (Message PB_Network)
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
