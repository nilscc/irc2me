{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Irc2me.ProtoBuf.Helper where

-- lens
import Control.Lens

-- bytestring
import Data.ByteString (ByteString)

-- text package
import           Data.Text          (Text)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE


decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8With EE.lenientDecode

encodeUtf8 :: Text -> ByteString
encodeUtf8 = E.encodeUtf8

encoded :: Iso' ByteString Text
encoded = iso decodeUtf8 encodeUtf8
