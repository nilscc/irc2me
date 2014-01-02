{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module IRC.ProtoBuf where

import Data.ProtocolBuffers
import Data.TypeLevel
import Data.Text
import Data.Int
import Data.Word
--import Data.Monoid
import Data.Serialize

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)

import GHC.Generics (Generic)

import qualified IRC.Types as IRC

--------------------------------------------------------------------------------
-- Type

-- | Header: <uint8> <encoded message type>
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
