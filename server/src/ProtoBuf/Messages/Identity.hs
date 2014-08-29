{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtoBuf.Messages.Identity where

import Control.Lens hiding (Identity)
import Control.Applicative

import Data.Text (Text)
import Data.ProtocolBuffers
import Data.Monoid
import Data.Maybe

import GHC.Generics (Generic)

import IRC.Types

import ProtoBuf.Types
import ProtoBuf.Helper
import ProtoBuf.Instances ()

data PB_Identity = PB_Identity
  { _pb_ident_id          :: Optional 1  (Value ID_T)
  , _pb_ident_nick        :: Optional 10 (Value Text)
  , _pb_ident_nick_alt    :: Repeated 11 (Value Text)
  , _pb_ident_name        :: Optional 12 (Value Text)
  , _pb_ident_realname    :: Optional 13 (Value Text)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Identity
instance Decode PB_Identity

makeLenses ''PB_Identity

emptyIdentity :: Integral id => id -> PB_Identity
emptyIdentity i = PB_Identity
  (putField $ Just $ fromIntegral i)
  mempty
  mempty
  mempty
  mempty

encodeIdentity :: Identity -> PB_Identity
encodeIdentity ident = emptyIdentity (ident ^. ident_id)
  & pb_ident_nick     .~~ Just (ident ^. ident_nick)
  & pb_ident_nick_alt .~~       ident ^. ident_nick_alt
  & pb_ident_name     .~~ Just (ident ^. ident_name)
  & pb_ident_realname .~~ Just (ident ^. ident_realname)

decodeIdentity :: PB_Identity -> Identity
decodeIdentity pbident = Identity
  { _ident_id        =          maybe 0 fromIntegral $ pbident ^. pb_ident_id       . field
  , _ident_nick      =     encodeUtf8 $ fromMaybe "" $ pbident ^. pb_ident_nick     . field
  , _ident_nick_alt  = map encodeUtf8 $                pbident ^. pb_ident_nick_alt . field
  , _ident_name      =     encodeUtf8 $ fromMaybe "" $ pbident ^. pb_ident_name     . field
  , _ident_realname  =     encodeUtf8 $ fromMaybe "" $ pbident ^. pb_ident_realname . field
  }

------------------------------------------------------------------------------
-- Folds

identitiesWithID :: ReifiedFold PB_Identity PB_Identity
identitiesWithID = Fold id <* Fold (pb_ident_id . field . _Just)

identitiesWithoutID :: ReifiedFold PB_Identity PB_Identity
identitiesWithoutID = Fold id <* Fold (pb_ident_id . field . _Nothing)
