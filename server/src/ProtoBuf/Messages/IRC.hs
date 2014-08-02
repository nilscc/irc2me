{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ProtoBuf.Messages.IRC where

import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad

import Data.ProtocolBuffers
import Data.Monoid
import Data.Text          (Text)

import GHC.Generics (Generic)

import qualified Network.IRC.ByteString.Parser as I
import qualified IRC.Types                     as IRC
import           ProtoBuf.Instances ()
import           ProtoBuf.Helper

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
  | Ty_TopicMsg
  | Ty_NickMsg
  | Ty_NamreplyMsg
  | Ty_ErrorMsg
  | Ty_RawMsg
  deriving (Eq, Enum, Show)

data PB_Namreply = PB_Namreply
  { namreply_name     :: Required 1 (Value Text)
  , namreply_userflag :: Optional 2 (Enumeration IRC.Userflag)
  }
  deriving (Eq, Show, Generic)

instance Encode PB_Namreply
instance Decode PB_Namreply

data PB_IrcMessage = PB_IrcMessage
  { -- message type
    _irc_msg_type        :: Required 1  (Enumeration IrcMsgType)
    -- raw messages
  , _irc_msg_from        :: Optional 5 (Value Text) -- either...
  , _irc_msg_servername  :: Optional 6 (Value Text) -- ...or
  , _irc_msg_command     :: Optional 7 (Value Text)
  , _irc_msg_params      :: Repeated 8 (Value Text)
  , _irc_msg_content     :: Optional 9 (Value Text)
    -- privmsg/notice
  , _irc_msg_to          :: Optional 10 (Value Text)
    -- nick change + namreply
  , _irc_msg_new_nick    :: Optional 21 (Value Text)
  , _irc_msg_namreply    :: Repeated 22 (Message PB_Namreply)
    -- join/part/kick/quit
  , _irc_msg_channels    :: Repeated 30 (Value Text)
  , _irc_msg_who         :: Optional 31 (Value Text)
    -- motd/topic
  , _irc_msg_notopic     :: Optional 42 (Value Bool)
  }
  deriving (Show, Generic)

makeLenses ''PB_IrcMessage

instance Encode PB_IrcMessage
instance Decode PB_IrcMessage

emptyIrcMessage :: IrcMsgType -> PB_IrcMessage
emptyIrcMessage ty = PB_IrcMessage
  -- msg type
  (putField ty)
  -- raw messages
  mempty
  mempty
  mempty
  mempty
  mempty
  -- privmsg/notice
  mempty
  -- nick change
  mempty
  mempty
  -- join/part/kick
  mempty
  mempty
  -- motd/topic
  mempty

encodeIrcMessage :: IRC.Message -> PB_IrcMessage
encodeIrcMessage msg =
  case msg of
    IRC.PrivMsg from to cont ->
      emptyIrcMessage Ty_PrivMsg
        & irc_msg_from          .~~ maybeNick   from
        & irc_msg_servername    .~~ maybeServer from
        & irc_msg_to            .~~ Just to
        & irc_msg_content       .~~ Just cont
    IRC.NoticeMsg from to cont ->
      emptyIrcMessage Ty_NoticeMsg
        & irc_msg_from          .~~ maybeNick   from
        & irc_msg_servername    .~~ maybeServer from
        & irc_msg_to            .~~ Just to
        & irc_msg_content       .~~ Just cont
    IRC.JoinMsg chans (fmap I.userNick -> who) ->
      emptyIrcMessage Ty_JoinMsg
        & irc_msg_channels      .~~ chans
        & irc_msg_who           .~~ who
    IRC.PartMsg chan (fmap I.userNick -> who) ->
      emptyIrcMessage Ty_PartMsg
        & irc_msg_channels      .~~ [chan]
        & irc_msg_who           .~~ who
    IRC.KickMsg chan who comment ->
      emptyIrcMessage Ty_KickMsg
        & irc_msg_channels      .~~ [chan]
        & irc_msg_who           .~~ who
        & irc_msg_content       .~~ comment
    IRC.QuitMsg who comment ->
      emptyIrcMessage Ty_QuitMsg
        & irc_msg_who           .~~ I.userNick `fmap` who
        & irc_msg_content       .~~ comment
    IRC.MOTDMsg motd ->
      emptyIrcMessage Ty_MOTDMsg
        & irc_msg_content       .~~ Just motd
    IRC.TopicMsg chan topic ->
      emptyIrcMessage Ty_TopicMsg
        & irc_msg_channels      .~~ [chan]
        & irc_msg_content       .~~ topic
    IRC.NickMsg (fmap I.userNick -> old) new ->
      emptyIrcMessage Ty_NickMsg
        & irc_msg_from          .~~ old
        & irc_msg_new_nick      .~~ Just new
    IRC.NamreplyMsg chan names ->
      emptyIrcMessage Ty_NamreplyMsg
        & irc_msg_channels      .~~ [chan]
        & irc_msg_namreply      .~  putNamreply names
    IRC.ErrorMsg code ->
      emptyIrcMessage Ty_ErrorMsg
        & irc_msg_command       .~~ Just code
    IRC.OtherMsg mfrom cmd params content ->
       emptyIrcMessage Ty_RawMsg
         & irc_msg_from         .~~ join (maybeNick   `fmap` mfrom)
         & irc_msg_servername   .~~ join (maybeServer `fmap` mfrom)
         & irc_msg_command      .~~ Just cmd
         & irc_msg_params       .~~ params
         & irc_msg_content      .~~ Just content

--
-- Helper
--

putNamreply :: [(IRC.Nickname, Maybe IRC.Userflag)]
            -> Repeated a (Message PB_Namreply)
putNamreply n = putField $ map `flip` n $ \(name, uf) ->
  PB_Namreply (putField $ decodeUtf8 name) (putField uf)
