{-# LANGUAGE DeriveGeneric #-}

-- | Module for client to server messages
module IRC.ProtoBuf.Client where

import Data.ProtocolBuffers
import Data.TypeLevel.Num
import Data.Word

import GHC.Generics (Generic)

import IRC.ProtoBuf.Instances ()

data Request
  = SetOpMode
  | GetBacklog
  deriving (Eq, Enum, Show)

data OpMode
  = OpModeStandard
  | OpModeBackground
  deriving (Eq, Enum, Show)

data PB_Request = PB_Request
  { rq_request          :: Required D1  (Enumeration Request)
    -- mode changes
  , rq_opmode           :: Optional D10 (Enumeration OpMode)
    -- message transfer
  , rq_msg_max_backlog  :: Optional D20 (Value Word32)
  }
  deriving (Generic, Show)

instance Encode PB_Request
instance Decode PB_Request
