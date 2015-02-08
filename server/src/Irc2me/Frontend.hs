{-# LANGUAGE DataKinds #-}

module Irc2me.Frontend where

import Control.Monad.Trans

import System.IO
import Network

import Control.Concurrent.Event
import Irc2me.Events.Types
import Irc2me.Frontend.ServerStream

runFrontend
  :: MonadIO m
  => ClientID
  -> Handle
  -> HostName
  -> EventQueue WO Event
  -> m ()
runFrontend cid h hostname eq = do

  res <- runStream h eq $ serverStream cid
  case res of
    Right () -> return ()
    Left err -> liftIO $ putStrLn $ "[" ++ hostname ++ "] " ++ err

  -- raise disconnected event
  liftIO $ writeEventIO eq $
    ClientEvent cid ClientDisconnectedEvent
