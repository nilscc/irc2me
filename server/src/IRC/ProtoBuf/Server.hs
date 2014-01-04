{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module for server to client messages
module IRC.ProtoBuf.Server where

import Data.ProtocolBuffers
import Data.TypeLevel.Num
import Data.Word
import Data.Monoid

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as E

import GHC.Generics (Generic)

import qualified Network.IRC.ByteString.Parser as I
import qualified IRC.Types                     as IRC

--------------------------------------------------------------------------------
-- IRC messages

data IrcMsgType
  = Ty_PrivMsg
  | Ty_NoticeMsg
  | Ty_JoinMsg
  | Ty_PartMsg
  | Ty_KickMsg
  | Ty_QuitMsg
  | Ty_MOTDMsg
  | Ty_NickMsg
  | Ty_ErrorMsg
  deriving (Eq, Enum, Show)

data PB_IrcMessage = PB_IrcMessage
  { -- message type
    irc_msg_type        :: Required D1  (Enumeration IrcMsgType)
    -- privmsg/notice
  , irc_msg_from        :: Optional D10 (Value Text)
  , irc_msg_servername  :: Optional D11 (Value Text)
  , irc_msg_to          :: Optional D12 (Value Text)
  , irc_msg_content     :: Optional D13 (Value Text)
    -- nick change
  , irc_msg_nick_old    :: Optional D20 (Value Text)
  , irc_msg_nick_new    :: Optional D21 (Value Text)
    -- join/part/kick/quit
  , irc_msg_channels    :: Repeated D30 (Value Text)
  , irc_msg_who         :: Optional D31 (Value Text)
  , irc_msg_comment     :: Optional D32 (Value Text)
    -- motd/topic
  , irc_msg_motd        :: Optional D40 (Value Text)
  , irc_msg_topic       :: Optional D41 (Value Text)
  , irc_msg_notopic     :: Optional D42 (Value Bool)
    -- errors
  , irc_msg_code        :: Optional D99  (Value Word32)
  }
  deriving (Show, Generic)

instance Encode PB_IrcMessage
instance Decode PB_IrcMessage

emptyIrcMessage :: IrcMsgType -> PB_IrcMessage
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
  , irc_msg_channels    = mempty
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
        { irc_msg_channels = putBSs [chan]
        , irc_msg_who      = putBSMaybe who
        }
    IRC.PartMsg chan (fmap I.userNick -> who) ->
      (emptyIrcMessage Ty_PartMsg)
        { irc_msg_channels = putBSs [chan]
        , irc_msg_who      = putBSMaybe who
        }
    IRC.KickMsg chan who comment ->
      (emptyIrcMessage Ty_KickMsg)
        { irc_msg_channels = putBSs [chan]
        , irc_msg_who      = putBSMaybe who
        , irc_msg_comment  = putBSMaybe comment
        }
    IRC.QuitMsg chans who comment ->
      (emptyIrcMessage Ty_KickMsg)
        { irc_msg_channels = putBSs chans
        , irc_msg_who      = putBSMaybe (I.userNick `fmap` who)
        , irc_msg_comment  = putBSMaybe comment
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

putBSs :: [ByteString] -> Repeated a (Value Text)
putBSs = putField . map E.decodeUtf8

putBSMaybe :: Maybe ByteString -> Optional a (Value Text)
putBSMaybe = putField . fmap E.decodeUtf8
