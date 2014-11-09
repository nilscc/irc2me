{-# LANGUAGE TemplateHaskell #-}

module Irc2me.ProtoBuf.Messages.System where

import Control.Lens

data SystemMsg
  = SystemMsg_Disconnect
  | SystemMsg_PING
  | SystemMsg_PONG
  deriving (Eq, Enum, Show)

makePrisms ''SystemMsg
