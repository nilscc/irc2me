{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.Frontend.Messages.IrcIdentity where

import Control.Lens
import Control.Applicative

import Data.Text (Text)
import Data.ProtocolBuffers

import GHC.Generics (Generic)

-- protobuf
import Data.ProtocolBuffers.Orphans ()
import Data.ProtocolBuffers.TH

-- local
import Irc2me.Frontend.Helper

data IrcIdentity = IrcIdentity
  { _identityId          :: Optional 1  (Value ID_T)
  , _identityNick        :: Optional 10 (Value Text)
  , _identityNickAlt     :: Repeated 11 (Value Text)
  , _identityName        :: Optional 12 (Value Text)
  , _identityRealname    :: Optional 13 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode IrcIdentity
instance Decode IrcIdentity

emptyIrcIdentity :: IrcIdentity
emptyIrcIdentity = IrcIdentity
  { _identityId       = putField Nothing
  , _identityNick     = putField Nothing
  , _identityNickAlt  = putField []
  , _identityName     = putField Nothing
  , _identityRealname = putField Nothing
  }

------------------------------------------------------------------------------
-- Lenses

makeFieldLenses ''IrcIdentity

------------------------------------------------------------------------------
-- Folds

identitiesWithID :: ReifiedFold IrcIdentity IrcIdentity
identitiesWithID = Fold id <* Fold (identityId . _Just)

identitiesWithoutID :: ReifiedFold IrcIdentity IrcIdentity
identitiesWithoutID = Fold id <* Fold (identityId . _Nothing)
