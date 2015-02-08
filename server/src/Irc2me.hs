{-# LANGUAGE DataKinds #-}

module Irc2me where

import Control.Concurrent
import Control.Concurrent.Event
import Control.Exception
import Control.Monad

import System.Exit

import Network
import qualified Network.Socket as Socket
-- import Network.TLS as TLS

import Irc2me.Backends
import Irc2me.Events
import Irc2me.Frontend

--------------------------------------------------------------------------------
-- Handling incoming messages

data ServerConfig = ServerConfig
  { serverPort    :: PortNumber
  , serverUseTLS  :: Bool
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverPort = 6565
  , serverUseTLS = False -- FIXME
  }

runServer
  :: ServerConfig
  -> IO ()
runServer conf = do

  -- start event loop
  eq <- newEventQueue
  void $ forkIO $ runEventTRW eq handleEvents

  -- start backends
  putStrLn "Starting backends"

  success <- runEventTWO eq runBackends
  unless success $ exitFailure

  -- start frontend, accept client connections
  putStrLn $ "Starting server on " ++ show (serverPort conf)

  socket <- listenOn $ PortNumber (serverPort conf)

  finally `flip` Socket.close socket $ forM_ [1..] $ \cid -> do

    (h, hostname, _) <- accept socket

    void $ forkIO $ do
      putStrLn $ "[" ++ hostname ++ "] New connection."
      runFrontend cid h hostname eq
