{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Irc2me.Frontend.Messages.Authentication where

import Data.Text

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

data AuthenticationMessage = AuthenticationMessage
  { _authLogin    :: Required 1 (Value Text)
  , _authPassword :: Required 2 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode AuthenticationMessage
instance Decode AuthenticationMessage

makeFieldLenses ''AuthenticationMessage

emptyAuthenticationMessage :: Text -> Text -> AuthenticationMessage
emptyAuthenticationMessage l p = AuthenticationMessage
  { _authLogin = putField l
  , _authPassword = putField p
  }
