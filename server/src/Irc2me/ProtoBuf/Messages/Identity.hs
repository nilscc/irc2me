{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.ProtoBuf.Messages.Identity where

import Control.Lens hiding (Identity)
import Control.Applicative

import Data.Text (Text)
import Data.ProtocolBuffers
import Data.Int

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

type ID_T = Int64

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

makeFieldLenses ''Identity

emptyIdentity :: Identity
emptyIdentity = Identity
  { _identityId       = putField Nothing
  , _identityNick     = putField Nothing
  , _identityNickAlt  = putField []
  , _identityName     = putField Nothing
  , _identityRealname = putField Nothing
  }

------------------------------------------------------------------------------
-- Folds

identitiesWithID :: ReifiedFold Identity Identity
identitiesWithID = Fold id <* Fold (identityId . _Just)

identitiesWithoutID :: ReifiedFold Identity Identity
identitiesWithoutID = Fold id <* Fold (identityId . _Nothing)
