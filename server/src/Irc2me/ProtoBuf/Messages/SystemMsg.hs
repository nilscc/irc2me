module Irc2me.ProtoBuf.Messages.SystemMsg where

data SystemMsg
  = SystemMsg_Disconnect
  | SystemMsg_PING
  | SystemMsg_PONG
  deriving (Eq, Enum, Show)
