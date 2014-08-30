{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ProtoBuf.Messages.IRC where

-- import Data.ByteString (ByteString)
import Data.ProtocolBuffers
import Data.ProtocolBuffers.TH
import Data.Text          (Text)

import GHC.Generics (Generic)

-- lens package
import Control.Lens

--------------------------------------------------------------------------------
-- IRC messages

data IrcMessage = IrcMessage
  { _ircPrivateMessage  :: Optional 1 (Message PrivateMessage)
  , _ircStatusMessage   :: Optional 2 (Message StatusMessage)
  }

data PrivateMessage = PrivateMessage
  { _pmFromUser         :: Optional 1 (Message User)
  , _pmFromServer       :: Optional 2 (Value Text)
  , _pmTo               :: Repeated 3 (Value Text)
  , _pmMessage          :: Optional 4 (Value Text)
  }
  deriving (Show, Generic)

data Userflag = Operator | Voice
  deriving (Show, Eq, Ord, Enum)

data User = User
  { _userNick           :: Required 1 (Value Text)
  , _userName           :: Optional 2 (Value Text)
  , _userHost           :: Optional 3 (Value Text)
  , _userFlag           :: Optional 4 (Enumeration Userflag)
  }
  deriving (Show, Generic)

data StatusType
  = Notice
  | Join
  | Part
  | Quit
  | Kick
  | Nick
  | Topic
  | MessageOfTheDay
  deriving (Eq, Ord, Show, Enum)

data StatusMessage = StatusMessage
  { _smType         :: Required 1 (Enumeration StatusType)
  , _smFromUser     :: Optional 2 (Message User)
  , _smFromServer   :: Optional 3 (Value Text)
  , _smTo           :: Repeated 3 (Value Text)
  , _smMessage      :: Optional 4 (Value Text)
  }
  deriving (Show, Generic)

--
-- instances, TH etc
--

makeFieldLenses ''IrcMessage

makeFieldLenses ''User
makePrisms      ''Userflag

makeFieldLenses ''PrivateMessage

makeFieldLenses ''StatusMessage
makePrisms      ''StatusType

{-
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

ircMsgType :: Iso' ByteString IrcMsgType
ircMsgType = iso to' from'
 where
  to' = undefined
  from' = undefined

ircMessage :: Iso' I.IRCMsg PB_IrcMessage
ircMessage = iso to' from'
 where

  from' = undefined

  to' :: I.IRCMsg -> PB_IrcMessage
  to' msg = emptyIrcMessage ty &~ do
    setPrefix $ I.msgPrefix msg
    setParams $ I.msgParams msg
    setTrail  $ I.msgTrail  msg
   where
    ty = I.msgCmd msg ^. ircMsgType
    setPrefix Nothing     = return ()
    setPrefix (Just prfx) = case prfx of
      Left (I.UserInfo nick _ _) -> do
        irc_msg_from . field .= _Just # (nick ^. encoded)
      Right srv -> do
        irc_msg_servername . field .= (_Just # (srv ^. encoded))

    setCmd = undefined
    setParams = undefined
    setTrail = undefined

{-
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
        & irc_msg_namreply      .~~ toNamreply names
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

toNamreply :: [(IRC.Nickname, Maybe IRC.Userflag)]
           -> [PB_Namreply]
toNamreply n = map `flip` n $ \(name, uf) ->
  emptyNamreply & namreply_name     .~~ name
                & namreply_userflag .~~ uf
                -}
                -}
