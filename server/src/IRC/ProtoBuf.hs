{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-unused-binds #-}

module IRC.ProtoBuf
  ( -- * Protocol buffer encoding/decoding
    encodeIrcMessage, PB_IrcMessage
    -- ** Serialization
  , messageToBS, lazyBsToMessage
  ) where

import Data.ProtocolBuffers
import Data.TypeLevel           hiding (Bool, (+))
import Data.Text
import Data.Bits
import Data.Word
import Data.Monoid
import Data.Serialize

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BUILDER
import qualified Data.Text.Encoding           as E

import GHC.Generics (Generic)

import qualified Network.IRC.ByteString.Parser as I
import qualified IRC.Types                     as IRC

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

--------------------------------------------------------------------------------
-- Type

data MsgType
  = Ty_PrivMsg
  | Ty_NoticeMsg
  | Ty_JoinMsg
  | Ty_PartMsg
  | Ty_KickMsg
  | Ty_MOTDMsg
  | Ty_NickMsg
  | Ty_ErrorMsg
  deriving (Prelude.Eq, Enum, Show)

data PB_IrcMessage = PB_IrcMessage
  { -- msg type, irc codes etc.
    irc_msg_type        :: Required D1  (Enumeration MsgType)
  , irc_msg_code        :: Optional D2  (Value Word32)
    -- content
  , irc_msg_from        :: Optional D10 (Value Text)
  , irc_msg_servername  :: Optional D11 (Value Text)
  , irc_msg_to          :: Optional D12 (Value Text)
  , irc_msg_content     :: Optional D13 (Value Text)
    -- nick change
  , irc_msg_nick_old    :: Optional D20 (Value Text)
  , irc_msg_nick_new    :: Optional D21 (Value Text)
    -- join/part/kick
  , irc_msg_channel     :: Optional D30 (Value Text)
  , irc_msg_who         :: Optional D31 (Value Text)
  , irc_msg_comment     :: Optional D32 (Value Text)
    -- motd/topic
  , irc_msg_motd        :: Optional D40 (Value Text)
  , irc_msg_topic       :: Optional D41 (Value Text)
  , irc_msg_notopic     :: Optional D42 (Value Bool)
  }
  deriving (Show, Generic)

instance Encode PB_IrcMessage
instance Decode PB_IrcMessage

emptyIrcMessage :: MsgType -> PB_IrcMessage
emptyIrcMessage ty = PB_IrcMessage
  { -- msg type, irc codes etc.
    irc_msg_type        = putField ty
  , irc_msg_code        = mempty
    -- privmsg/notice
  , irc_msg_from        = mempty
  , irc_msg_servername  = mempty
  , irc_msg_to          = mempty
  , irc_msg_content     = mempty
    -- nick change
  , irc_msg_nick_old    = mempty
  , irc_msg_nick_new    = mempty
    -- join/part/kick
  , irc_msg_channel     = mempty
  , irc_msg_who         = mempty
  , irc_msg_comment     = mempty
    -- motd/topic
  , irc_msg_motd        = mempty
  , irc_msg_topic       = mempty
  , irc_msg_notopic     = mempty
  }

encodeIrcMessage :: IRC.Message -> PB_IrcMessage
encodeIrcMessage msg =
  case msg of
    IRC.PrivMsg from to cont ->
      let (nick,srv) = splitFrom from
       in (emptyIrcMessage Ty_PrivMsg)
            { irc_msg_from       = nick
            , irc_msg_servername = srv
            , irc_msg_to         = putBS to
            , irc_msg_content    = putBS cont
            }
    IRC.NoticeMsg from to cont ->
      let (nick,srv) = splitFrom from
       in (emptyIrcMessage Ty_NoticeMsg)
            { irc_msg_from       = nick
            , irc_msg_servername = srv
            , irc_msg_to         = putBS to
            , irc_msg_content    = putBS cont
            }
    IRC.JoinMsg chan (fmap I.userNick -> who) ->
      (emptyIrcMessage Ty_JoinMsg)
        { irc_msg_channel = putBS chan
        , irc_msg_who     = putBSMaybe who
        }
    IRC.PartMsg chan (fmap I.userNick -> who) ->
      (emptyIrcMessage Ty_PartMsg)
        { irc_msg_channel = putBS chan
        , irc_msg_who     = putBSMaybe who
        }
    IRC.KickMsg chan who comment ->
      (emptyIrcMessage Ty_KickMsg)
        { irc_msg_channel = putBS chan
        , irc_msg_who     = putBSMaybe who
        , irc_msg_comment = putBSMaybe comment
        }
    IRC.MOTDMsg motd ->
      (emptyIrcMessage Ty_MOTDMsg)
        { irc_msg_motd = putBS motd
        }
    IRC.NickMsg (fmap I.userNick -> old) new ->
      (emptyIrcMessage Ty_NickMsg)
        { irc_msg_nick_old = putBSMaybe old
        , irc_msg_nick_new = putBS new
        }
    IRC.ErrorMsg code ->
      (emptyIrcMessage Ty_ErrorMsg)
        { irc_msg_code = putField $ Just (fromIntegral code)
        }

--
-- helper
--

splitFrom
  :: Either I.UserInfo ByteString
  -> (Optional a (Value Text), Optional b (Value Text))
splitFrom from =
  case from of
    Left ui -> (putField $ Just $ E.decodeUtf8 (I.userNick ui), mempty)
    Right s -> (mempty, putField $ Just $ E.decodeUtf8 s)

putBS :: ByteString -> Optional a (Value Text)
putBS = putField . Just . E.decodeUtf8

putBSMaybe :: Maybe ByteString -> Optional a (Value Text)
putBSMaybe = putField . fmap E.decodeUtf8
