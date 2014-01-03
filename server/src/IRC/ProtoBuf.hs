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
  , messageToBS, bsToMessage
  ) where

import Data.ProtocolBuffers
import Data.TypeLevel           hiding (Bool)
import Data.Text
import Data.Word
import Data.Monoid
import Data.Serialize

import           Data.ByteString    (ByteString)
import qualified Data.Text.Encoding as E

import GHC.Generics (Generic)

import qualified Network.IRC.ByteString.Parser as I
import qualified IRC.Types                     as IRC

messageToBS :: Encode a => a -> ByteString
messageToBS = runPut . encodeMessage

bsToMessage :: Decode a => ByteString -> Either String a
bsToMessage = runGet decodeMessage

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
    -- content
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
