{-# LANGUAGE PatternGuards #-}

module IRC.ProtoBuf
  ( -- * Protocol buffer encoding/decoding
    encodeIrcMessage, PB_IrcMessage
    -- ** Serialization
  , messageToBS, lazyBsToMessage
  ) where

import Data.ProtocolBuffers
import Data.Bits
import Data.Word
import Data.Serialize

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BUILDER

import IRC.ProtoBuf.Server

-- | Encode a message with a 32-bit unsigned integer (big-endian encoded)
-- length prefix
messageToBS :: Encode a => a -> ByteString
messageToBS m =
  let bs  = runPut $ encodeMessage m
      len = fromIntegral (BS.length bs) :: Word32
   in w32toBS len `BS.append` bs

w32toBS :: Word32 -> ByteString
w32toBS w32 = BL.toStrict $
  BUILDER.toLazyByteString (BUILDER.word32BE w32)

-- | Decode a lazy "BL.ByteString" with 32-bit unsigned integer (big-endian
-- encoded) length prefix. Returns the rest of the "BL.ByteString"
lazyBsToMessage
  :: Decode a => BL.ByteString -> Either String (a, BL.ByteString)
lazyBsToMessage bs
  | Just (len, bs') <- bsToW32 bs =
    let (bs_msg, bs_rst) = BL.splitAt (fromIntegral len) bs'
     in case runGetLazy decodeMessage bs_msg of
          Right a  -> Right (a, bs_rst)
          Left err -> Left err
  | otherwise =
    Left "No length prefix found. ByteString too short?"

bsToW32 :: BL.ByteString -> Maybe (Word32, BL.ByteString)
bsToW32 bs
  | Just (w1,bs1) <- BL.uncons bs
  , Just (w2,bs2) <- BL.uncons bs1
  , Just (w3,bs3) <- BL.uncons bs2
  , Just (w4,bs4) <- BL.uncons bs3
  = Just (buildW32 (fromIntegral w1)
                   (fromIntegral w2)
                   (fromIntegral w3)
                   (fromIntegral w4), bs4)
  | otherwise
  = Nothing
 where
  buildW32 w1 w2 w3 w4 =
      shift w1 24
    + shift w2 16
    + shift w3 8
    + w4
