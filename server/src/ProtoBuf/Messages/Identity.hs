{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module ProtoBuf.Messages.Identity where

import Data.ProtocolBuffers

import GHC.Generics (Generic)

import ProtoBuf.Instances ()

data PB_Identity = PB_Identity
  {
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Identity
instance Decode PB_Identity
