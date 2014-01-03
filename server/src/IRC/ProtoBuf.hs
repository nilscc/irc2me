{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-unused-binds #-}
module IRC.ProtoBuf
  ( encodeIrcMessage
  ) where

import Data.ProtocolBuffers
import Data.TypeLevel
import Data.Text
import Data.Word
import Data.Monoid
import Data.Serialize

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BUILDER

import qualified Data.Text.Encoding as E

import GHC.Generics (Generic)

import qualified Network.IRC.ByteString.Parser as I
import qualified IRC.Types                     as IRC

encodeIrcMessage :: IRC.Message -> ByteString
encodeIrcMessage msg = mkHeader msg `BS.append` mkBody msg

-- | Header with length prefix: <uint8> <encoded message type>
mkHeader :: IRC.Message -> ByteString
mkHeader msg =
  let pbm = IrcMessage (putField ty)
      enc = runPut $ encodeMessage pbm
      len = fromIntegral (BS.length enc) :: Word8
   in len `BS.cons` enc
 where
  ty = case msg of
         IRC.PrivMsg {} -> Ty_PrivMsg
         IRC.NoticeMsg {} -> Ty_NoticeMsg
         IRC.JoinMsg {} -> Ty_JoinMsg
         IRC.PartMsg {} -> Ty_PartMsg
         IRC.KickMsg {} -> Ty_KickMsg
         IRC.MOTDMsg {} -> Ty_MOTDMsg
         IRC.NickMsg {} -> Ty_NickMsg
         IRC.ErrorMsg {} -> Ty_ErrorMsg

-- | Body with length prefix: <uint32> <encoded message body>
mkBody :: IRC.Message -> ByteString
mkBody msg =
  let bs  = runPut $ putMsg msg
      len = fromIntegral (BS.length bs) :: Word32
   in w32toBS len `BS.append` bs

w32toBS :: Word32 -> ByteString
w32toBS w32 = BL.toStrict $
  BUILDER.toLazyByteString (BUILDER.word32LE w32)

putMsg :: IRC.Message -> Put
putMsg msg =
  case msg of
       (toPrivMsg   -> Just m) -> encodeMessage m
       (toNoticeMsg -> Just m) -> encodeMessage m
       (toJoinMsg   -> Just m) -> encodeMessage m
       (toPartMsg   -> Just m) -> encodeMessage m
       (toMOTDMsg   -> Just m) -> encodeMessage m
       (toNickMsg   -> Just m) -> encodeMessage m
       (toErrorMsg  -> Just m) -> encodeMessage m
       _ -> error "putMsg: No conversion!"

--------------------------------------------------------------------------------
-- Conversion

toUserInfo :: I.UserInfo -> UserInfo
toUserInfo (I.UserInfo nick name host) =
  UserInfo (putField $ E.decodeUtf8 nick)
           (putField $ E.decodeUtf8 `fmap` name)
           (putField $ E.decodeUtf8 `fmap` host)

toPrivMsg :: IRC.Message -> Maybe PrivMsg
toPrivMsg (IRC.PrivMsg from to content) = Just $
  PrivMsg ui srv
          (putField $ E.decodeUtf8 to)
          (putField $ E.decodeUtf8 content)
 where
  (ui,srv) = splitFrom from
toPrivMsg _ = Nothing

toNoticeMsg :: IRC.Message -> Maybe NoticeMsg
toNoticeMsg (IRC.NoticeMsg from to content) = Just $
  NoticeMsg ui srv
            (putField $ E.decodeUtf8 to)
            (putField $ E.decodeUtf8 content)
 where
  (ui,srv) = splitFrom from
toNoticeMsg _ = Nothing

toJoinMsg :: IRC.Message -> Maybe JoinMsg
toJoinMsg (IRC.JoinMsg chan who) = Just $
  JoinMsg (putField $ E.decodeUtf8 chan)
          (putField $ toUserInfo `fmap` who)
toJoinMsg _ = Nothing

toPartMsg :: IRC.Message -> Maybe PartMsg
toPartMsg (IRC.PartMsg chan who) = Just $
  PartMsg (putField $ E.decodeUtf8 chan)
          (putField $ toUserInfo `fmap` who)
toPartMsg _ = Nothing

toKickMsg :: IRC.Message -> Maybe KickMsg
toKickMsg (IRC.KickMsg chan who comment) = Just $
  KickMsg (putField $ E.decodeUtf8 chan)
          (putField $ E.decodeUtf8 `fmap` who)
          (putField $ E.decodeUtf8 `fmap` comment)
toKickMsg _ = Nothing

toMOTDMsg :: IRC.Message -> Maybe MOTDMsg
toMOTDMsg (IRC.MOTDMsg motd) = Just $
  MOTDMsg (putField $ E.decodeUtf8 motd)
toMOTDMsg _ = Nothing

toNickMsg :: IRC.Message -> Maybe NickMsg
toNickMsg (IRC.NickMsg old new) = Just $
  NickMsg (putField $ toUserInfo `fmap` old)
          (putField $ E.decodeUtf8 new)
toNickMsg _ = Nothing

toErrorMsg :: IRC.Message -> Maybe ErrorMsg
toErrorMsg (IRC.ErrorMsg err) = Just $
  ErrorMsg (putField $ fromIntegral err)
toErrorMsg _ = Nothing

--
-- Helper
--

splitFrom
  :: Either I.UserInfo ByteString
  -> (Optional a (Message UserInfo), Optional b (Value Text))
splitFrom from =
  case from of
    Left ui -> (putField $ Just $ toUserInfo ui, mempty)
    Right s -> (mempty, putField $ Just $ E.decodeUtf8 s)

--------------------------------------------------------------------------------
-- Type

data Type
  = Ty_PrivMsg
  | Ty_NoticeMsg
  | Ty_JoinMsg
  | Ty_PartMsg
  | Ty_KickMsg
  | Ty_MOTDMsg
  | Ty_NickMsg
  | Ty_ErrorMsg
  deriving (Prelude.Eq, Enum, Show)

data IrcMessage = IrcMessage
  { irc_msg_type :: Required D1 (Enumeration Type)
  }
  deriving (Show, Generic)

instance Encode IrcMessage
instance Decode IrcMessage

--------------------------------------------------------------------------------
-- User info

data UserInfo = UserInfo
  { irc_userinfo_nick :: Required D1 (Value Text)
  , irc_userinfo_name :: Optional D2 (Value Text)
  , irc_userinfo_host :: Optional D3 (Value Text)
  }
  deriving (Show, Generic)

instance Encode UserInfo
instance Decode UserInfo

--------------------------------------------------------------------------------
-- Messages

data PrivMsg = PrivMsg
  { irc_priv_from_user   :: Optional D1 (Message UserInfo)
  , irc_priv_from_server :: Optional D2 (Value Text)
  , irc_priv_to          :: Required D3 (Value Text)
  , irc_priv_content     :: Required D4 (Value Text)
  }
  deriving (Generic, Show)

instance Encode PrivMsg
instance Decode PrivMsg

data NoticeMsg = NoticeMsg
  { irc_notice_from_user   :: Optional D1 (Message UserInfo)
  , irc_notice_from_server :: Optional D2 (Value Text)
  , irc_notice_to          :: Required D3 (Value Text)
  , irc_notice_content     :: Required D4 (Value Text)
  }
  deriving (Generic, Show)

instance Encode NoticeMsg
instance Decode NoticeMsg

data JoinMsg = JoinMsg
  { irc_join_channel     :: Required D1 (Value Text)
  , irc_join_who         :: Optional D2 (Message UserInfo)
  }
  deriving (Generic, Show)

instance Encode JoinMsg
instance Decode JoinMsg

data PartMsg = PartMsg
  { irc_part_channel     :: Required D1 (Value Text)
  , irc_part_who         :: Optional D2 (Message UserInfo)
  }
  deriving (Generic, Show)

instance Encode PartMsg
instance Decode PartMsg

data KickMsg = KickMsg
  { irc_kick_channel     :: Required D1 (Value Text)
  , irc_kick_who         :: Optional D2 (Value Text)
  , irc_kick_comment     :: Optional D3 (Value Text)
  }
  deriving (Generic, Show)

instance Encode KickMsg
instance Decode KickMsg

data MOTDMsg = MOTDMsg
  { irc_motd             :: Required D1 (Value Text)
  }
  deriving (Generic, Show)

instance Encode MOTDMsg
instance Decode MOTDMsg

data NickMsg = NickMsg
  { irc_nick_old         :: Optional D1 (Message UserInfo)
  , irc_nick_new         :: Required D2 (Value Text)
  }
  deriving (Generic, Show)

instance Encode NickMsg
instance Decode NickMsg

data ErrorMsg = ErrorMsg
  { irc_error_code       :: Required D1 (Value Word32)
  }
  deriving (Generic, Show)

instance Encode ErrorMsg
instance Decode ErrorMsg
