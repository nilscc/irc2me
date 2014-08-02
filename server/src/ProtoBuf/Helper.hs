{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module ProtoBuf.Helper where

import Control.Monad
import Control.Lens.Lens
import Control.Lens.Setter
import Control.Lens.Type

import           Data.ProtocolBuffers
import           Data.ByteString (ByteString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE

import qualified Network.IRC.ByteString.Parser as I

--
-- helper
--
decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8With EE.lenientDecode

maybeNick, maybeServer :: Either I.UserInfo ByteString -> Maybe ByteString
maybeNick (Left ui)   = Just $ I.userNick ui
maybeNick _           = Nothing
maybeServer (Right s) = Just s
maybeServer _         = Nothing

--
-- lens helpers
--

-- | Field lens setter, minimal complete definition: `fieldGetter` and
-- `fieldSetter`
class FieldLens t t' where

  infixr 4 .~~
  (.~~) :: (HasField a, FieldType a ~ t)
        => ASetter' s a
        -> t' -> s -> s
  lns .~~ val = lns . field . asT .~ val

  asT :: Simple Lens t t'
  asT = lens fieldGetter fieldSetter

  fieldGetter :: t -> t'
  fieldSetter :: t -> t' -> t

-- general instances

instance FieldLens a a where
  fieldGetter = id
  fieldSetter = const id

instance (FieldLens t t', Monad m) => FieldLens (m t) (m t') where
  fieldGetter        = liftM fieldGetter
  fieldSetter ft ft' = (liftM fieldSetter ft) >>= flip liftM ft'

-- specific conversion instances

instance FieldLens Text ByteString where
  fieldGetter = E.encodeUtf8
  fieldSetter = const decodeUtf8
