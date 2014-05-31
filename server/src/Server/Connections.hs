module Server.Connections where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Network
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Serialize
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal

import ProtoBuf.Messages.Client

data ServerConf = ServerConf
  { serverPort    :: PortNumber
  }

defaultServerConf :: ServerConf
defaultServerConf = ServerConf
  { serverPort = 6565
  }

serverStart :: ServerConf -> IO ()
serverStart conf = do

  socket <- listenOn (PortNumber $ serverPort conf)
  forever $ do

    (h, hostname, _) <- accept socket

    forkIO $ do
      putStrLn $ "New connection from " ++ hostname
      stream <- BL.hGetContents h
      handleClientMessages (BL.toChunks stream) `finally` hClose h

handleClientMessages :: [B.ByteString] -> IO ()
handleClientMessages chunks =

  handleChunks chunks $ runGetPartial getVarintPrefixedBS

 where

  handleChunks (chunk : rest) f
    | B.null chunk = handleChunks rest f
    | otherwise =

      -- parse chunk
      case f chunk of

        Fail err _     -> putStrLn $ "Unexpected error: " ++ show err
        Partial f'     ->
          if null rest
            then putStrLn $ "Unexpected end of input after 'Partial' results."
            else handleChunks rest f'
        Done bs chunk' -> do

          -- try to parse current message
          case runGet decodeMessage bs of
            Left err  -> putStrLn $ "Failed to parse message: " ++ show err
            Right msg -> do
              handleClientMessage msg

          -- loop recursively
          handleClientMessages (chunk' : rest)

  handleChunks [] _ = return()

handleClientMessage :: PB_ClientMessage -> IO ()
handleClientMessage msg = do

  putStrLn $ "Incoming message: " ++ show msg
