{-# LANGUAGE OverloadedStrings #-}

module Server.Connections where

import Control.Exception
import Control.Concurrent
import Control.Monad

import           Network
import qualified Network.Socket as Socket

import Server.Streams
import Server.Streams.Server

data ServerConf = ServerConf
  { serverPort    :: PortNumber
  }

defaultServerConf :: ServerConf
defaultServerConf = ServerConf
  { serverPort = 6565
  }

-- | Accept incoming connections and process client messages
serverStart :: ServerConf -> IO ()
serverStart conf = do

  socket <- listenOn (PortNumber $ serverPort conf)

  finally `flip` Socket.close socket $
    forever $ do
      (h, hostname, _) <- accept socket

      forkIO $ do

        logWithHost hostname "New connection."
        res <- runStreamOnHandle h serverStream
        case res of
          Right () -> return ()
          Left err -> logWithHost hostname err

logWithHost :: String -> String -> IO ()
logWithHost host what = putStrLn $ "[" ++ host ++ "] " ++ what
