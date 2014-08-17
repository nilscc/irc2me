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
import ProtoBuf.Messages.Network
import ProtoBuf.Messages.Identity
import ProtoBuf.Messages.SystemMsg

data PB_List
  = PB_ListIdentities
  | PB_ListNetworks
  | PB_ListChannels
  deriving (Eq, Enum, Show)

data PB_ClientMessage = PB_ClientMessage
  { -- system
    _client_response_id     :: Optional 3  (Value ID_T)
  , _client_system_msg      :: Optional 5  (Enumeration PB_SystemMsg)
    -- acount
  , _auth_login             :: Optional 10 (Value Text)
  , _auth_password          :: Optional 11 (Value Text)

    -- identities
  , _ident_set              :: Repeated 20  (Message PB_Identity)
  , _ident_get_new          :: Optional 21  (Value Bool)
  , _ident_remove           :: Repeated 22  (Value ID_T)
  , _ident_get_all          :: Optional 23  (Value Bool)

    -- networks
  , _network_set            :: Repeated 100 (Message PB_Network)
  , _network_get_new        :: Optional 101 (Value Bool)
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
  -- system
  mempty
  mempty
  -- account
  mempty
  mempty
  -- identities
  mempty
  mempty
  mempty
  mempty
  -- networks
  mempty
  mempty
  mempty
  mempty
  mempty
