{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Irc2me.Frontend.Messages.Helper where

import Data.Int
import Data.Time.Clock
import Data.Time.Clock.POSIX

-- lens
import Control.Lens

-- bytestring
import Data.ByteString (ByteString)

-- text package
import           Data.Text          (Text)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE

type ID_T = Int64

decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8With EE.lenientDecode

encodeUtf8 :: Text -> ByteString
encodeUtf8 = E.encodeUtf8

encoded :: Iso' ByteString Text
encoded = iso decodeUtf8 encodeUtf8

epoch :: Iso' UTCTime Int64
epoch = iso toEpoch fromEpoch
 where
  fromEpoch e = posixSecondsToUTCTime $ fromIntegral e / 1000
  toEpoch   t = floor $ utcTimeToPOSIXSeconds t * 1000
