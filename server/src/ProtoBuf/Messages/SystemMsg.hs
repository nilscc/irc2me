module ProtoBuf.Messages.SystemMsg where

data PB_SystemMsg
  = PB_SystemMsg_Disconnect
  | PB_SystemMsg_PING
  | PB_SystemMsg_PONG
  deriving (Eq, Enum, Show)
