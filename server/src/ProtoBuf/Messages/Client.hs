{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for client to server messages
module ProtoBuf.Messages.Client where

import Control.Lens.TH

import Data.ProtocolBuffers
import Data.Text
import Data.Monoid

import GHC.Generics (Generic)

import ProtoBuf.Types
import ProtoBuf.Instances ()
import ProtoBuf.Messages.Identity
import ProtoBuf.Messages.SystemMsg

data PB_List
  = PB_ListIdentities
  | PB_ListNetworks
  | PB_ListChannels
  deriving (Eq, Enum, Show)

data PB_ClientMessage = PB_ClientMessage
  { -- acount
    _auth_login             :: Optional 1 (Value Text)
  , _auth_password          :: Optional 2 (Value Text)

    -- system
  , _client_system_msg      :: Optional 5 (Enumeration PB_SystemMsg)

    -- identities
  , _ident_add              :: Repeated 11  (Message PB_Identity)
  , _ident_remove           :: Repeated 12  (Value ID_T)
  , _ident_get_all          :: Optional 13  (Value Bool)

    -- networks
  , _network_add            :: Repeated 101 (Value Text)
  , _network_remove         :: Repeated 102 (Value ID_T)

  , _network_get_all_names  :: Optional 103 (Value Bool)
  , _network_get_details    :: Repeated 104 (Value ID_T)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_ClientMessage
instance Decode PB_ClientMessage

makeLenses ''PB_ClientMessage

emptyClientMessage :: PB_ClientMessage
emptyClientMessage = PB_ClientMessage
  -- account
  mempty
  mempty
  -- system
  mempty
  -- identities
  mempty
  mempty
  mempty
  -- networks
  mempty
  mempty
  mempty
  mempty
