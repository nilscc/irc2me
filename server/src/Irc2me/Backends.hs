{-# LANGUAGE DataKinds #-}

module Irc2me.Backends where

import Control.Monad.Trans
import Control.Concurrent.Event

import Irc2me.Events
import Irc2me.Backends.IRC

runBackends :: MonadIO m => EventT mode AccountEvent m Bool
runBackends = do
  eq <- getEventQueue
  runBackends' eq

runBackends' :: MonadIO m => EventQueue WO AccountEvent -> m Bool
runBackends' eq = do
  runIrcBackend' eq
