{-# LANGUAGE RankNTypes #-}

module Irc2me.Events.Types where

import Data.Aeson
import Control.Monad.Trans

type AccountID = Integer

data AccountEvent = AccountEvent AccountID Event

data Event
  = ClientConnected
    { sendToClient :: forall a m. (ToJSON a, MonadIO m) => a -> m ()
    }
