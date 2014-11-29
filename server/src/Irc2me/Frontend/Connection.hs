{-# LANGUAGE DataKinds #-}

module Irc2me.Frontend.Connection where

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
import Irc2me.Frontend.Streams

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
  eq <- newEventQueue
  runServer' conf eq

runServer'
  :: ServerConfig
  -> EventQueue WO AccountEvent
  -> IO ()
runServer' conf eq = do

  -- backends
  putStrLn "Starting backends"

  success <- runBackends' eq
  unless success $ exitFailure

  -- frontend
  putStrLn $ "Starting server on " ++ show (serverPort conf)

  socket <- listenOn $ PortNumber (serverPort conf)

  finally `flip` Socket.close socket $ forever $ do

    (h, hostname, _) <- accept socket

    forkIO $ do

      putStrLn $ "[" ++ hostname ++ "] New connection."

      res <- runStream h eq serverStream
      case res of
        Right () -> return ()
        Left err -> putStrLn $ "[" ++ hostname ++ "] " ++ err
