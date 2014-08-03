{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ProtoBuf.Messages.SystemMsg where

import Data.Int

import ProtoBuf.Helper

data PB_SystemMsg
  = PB_SystemMsg_Disconnect
  | PB_SystemMsg_PING
  | PB_SystemMsg_PONG
  deriving (Eq, Enum, Show)

toSystemMsg :: Int32 -> PB_SystemMsg
toSystemMsg = toEnum . fromIntegral

instance Convertible Int32 PB_SystemMsg where
  convert = fromIntegral . fromEnum
instance Functor f => Convertible (f Int32) (f PB_SystemMsg) where
  convert = fmap convert
