module Irc2me.Events
  ( handleEvents
  , module Irc2me.Events.Types
  ) where

import Control.Monad.Trans
import System.IO

-- local

import Control.Concurrent.Event

import Irc2me.Events.Types

handleEvents :: EventRW IO ()
handleEvents = do

  AccountEvent _account e <- getEvent
  case e of
    ClientConnectedEvent _send -> return ()
    _ -> liftIO $ hPutStrLn stderr $ "Unhandled event: " ++ show e
