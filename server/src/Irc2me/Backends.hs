{-# LANGUAGE DataKinds #-}

module Irc2me.Backends where

import Control.Monad.Trans
import Control.Concurrent.Event

import Irc2me.Events.Types
import Irc2me.Backends.IRC

runBackends :: MonadIO m => EventT mode Event m Bool
runBackends = do
  runIrcBackend
