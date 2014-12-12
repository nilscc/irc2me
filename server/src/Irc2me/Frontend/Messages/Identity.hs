{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.Frontend.Messages.Identity where

import Control.Lens hiding (Identity)
import Control.Applicative

import Data.Text (Text)
import Data.ProtocolBuffers

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

-- local
import Irc2me.Frontend.Messages.Helper

data Identity = Identity
  { _identityId          :: Optional 1  (Value ID_T)
  , _identityNick        :: Optional 10 (Value Text)
  , _identityNickAlt     :: Repeated 11 (Value Text)
  , _identityName        :: Optional 12 (Value Text)
  , _identityRealname    :: Optional 13 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode Identity
instance Decode Identity

emptyIdentity :: Identity
emptyIdentity = Identity
  { _identityId       = putField Nothing
  , _identityNick     = putField Nothing
  , _identityNickAlt  = putField []
  , _identityName     = putField Nothing
  , _identityRealname = putField Nothing
  }

------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''Identity

------------------------------------------------------------------------------
-- Folds

identitiesWithID :: ReifiedFold Identity Identity
identitiesWithID = Fold id <* Fold (identityId . _Just)

identitiesWithoutID :: ReifiedFold Identity Identity
identitiesWithoutID = Fold id <* Fold (identityId . _Nothing)
