{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ProtoBuf.Helper where

import Control.Lens.Setter

import           Data.Int (Int32, Int64)
import           Data.ProtocolBuffers
import           Data.ByteString (ByteString)
import           Data.Text          (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE

import qualified Network.IRC.ByteString.Parser as I

--
-- helper
--

decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8With EE.lenientDecode

encodeUtf8 :: Text -> ByteString
encodeUtf8 = E.encodeUtf8

maybeNick, maybeServer :: Either I.UserInfo ByteString -> Maybe ByteString
maybeNick (Left ui)   = Just $ I.userNick ui
maybeNick _           = Nothing
maybeServer (Right s) = Just s
maybeServer _         = Nothing

--
-- lens helpers
--

infixr 4 .~~
(.~~) :: (HasField a, FieldType a ~ t, Convertible t t')
      => ASetter' s a
      -> t' -> s -> s
lns .~~ val = lns . field .~ convert val

class Convertible t t' where
  convert :: t' -> t

instance Convertible a a where
  convert = id

instance Convertible Text ByteString where
  convert = decodeUtf8
instance Functor f => Convertible (f Text) (f ByteString) where
  convert = fmap convert

instance Convertible Text String where
  convert = Text.pack
instance Functor f => Convertible (f Text) (f String) where
  convert = fmap convert

instance Integral a => Convertible Int32 a where
  convert = fromIntegral
instance (Integral a, Functor f) => Convertible (f Int32) (f a) where
  convert = fmap convert

instance Integral a => Convertible Int64 a where
  convert = fromIntegral
instance (Integral a, Functor f) => Convertible (f Int64) (f a) where
  convert = fmap convert

