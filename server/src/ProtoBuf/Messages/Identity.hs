{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ProtoBuf.Messages.Identity where

import Control.Lens.Operators
import Control.Lens.TH

import Data.Text (Text)
import Data.ProtocolBuffers
import Data.Monoid
import Data.Maybe

import GHC.Generics (Generic)

import IRC.Types (Identity(..))

import ProtoBuf.Types
import ProtoBuf.Helper
import ProtoBuf.Instances ()

data PB_Identity = PB_Identity
  { _pb_ident_id          :: Required 1  (Value ID_T)
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
  (putField $ fromIntegral i)
  mempty
  mempty
  mempty
  mempty

encodeIdentity :: Identity -> PB_Identity
encodeIdentity ident = emptyIdentity (ident_id ident)
  & pb_ident_nick     .~~ Just (ident_nick     ident)
  & pb_ident_nick_alt .~~       ident_nick_alt ident
  & pb_ident_name     .~~ Just (ident_name     ident)
  & pb_ident_realname .~~ Just (ident_realname ident)

decodeIdentity :: PB_Identity -> Identity
decodeIdentity pbident = Identity
  { ident_id        =   fromIntegral $                pbident ^. pb_ident_id       . field
  , ident_nick      =     encodeUtf8 $ fromMaybe "" $ pbident ^. pb_ident_nick     . field
  , ident_nick_alt  = map encodeUtf8 $                pbident ^. pb_ident_nick_alt . field
  , ident_name      =     encodeUtf8 $ fromMaybe "" $ pbident ^. pb_ident_name     . field
  , ident_realname  =     encodeUtf8 $ fromMaybe "" $ pbident ^. pb_ident_realname . field
  }
