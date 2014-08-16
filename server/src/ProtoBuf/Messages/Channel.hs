{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ProtoBuf.Messages.Channel where

import Control.Lens.TH
import Data.ProtocolBuffers
import Data.Int
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)

data PB_Channel = PB_Channel
  { _channel_id       :: Required 1  (Value Int64)
  , _channel_online   :: Optional 5  (Value Bool)
  , _channel_name     :: Optional 10 (Value Text)
  }
  deriving (Show, Generic, Eq)

instance Encode PB_Channel
instance Decode PB_Channel

makeLenses ''PB_Channel

emptyChannel :: Integer -> PB_Channel
emptyChannel ch_id = PB_Channel
  (putField $ fromIntegral ch_id)
  mempty
  mempty
