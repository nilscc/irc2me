{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module for client to server messages
module Irc2me.Frontend.Messages.Client where

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

-- local
import Irc2me.Frontend.Messages.Helper
import Irc2me.Frontend.Messages.IrcNetwork
import Irc2me.Frontend.Messages.System

data ClientMessage = ClientMessage

    -- system
  { _clientResponseID       :: Optional 3  (Value ID_T)
  , _clientSystemMsg        :: Optional 5  (Enumeration SystemMsg)

    -- networks
  , _clientNetworks         :: Repeated 50 (Message IrcNetwork)

  }
  deriving (Eq, Show, Generic)

instance Encode ClientMessage
instance Decode ClientMessage

emptyClientMessage :: ClientMessage
emptyClientMessage = ClientMessage
  { _clientResponseID     = putField Nothing
  , _clientSystemMsg      = putField Nothing
  , _clientNetworks       = putField []
  }

------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''ClientMessage
