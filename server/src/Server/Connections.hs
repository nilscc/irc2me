{-# LANGUAGE OverloadedStrings #-}

module Server.Connections where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Network
import System.IO

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Serialize
import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal

import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server

import Database.Query
import Database.Tables.Accounts

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
      handleClientMessages h (BL.toChunks stream) `finally` hClose h

handleClientMessages :: Handle -> [B.ByteString] -> IO ()
handleClientMessages h chunks =

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
              -- send response
              srvmsg <- handleClientMessage msg
              let encoded = runPut $ encodeMessage srvmsg
              B.hPut h $ runPut $ putVarintPrefixedBS encoded


          -- loop recursively
          handleClientMessages h (chunk' : rest)

  handleChunks [] _ = return()

handleClientMessage :: PB_ClientMessage -> IO PB_ServerMessage
handleClientMessage msg

  | Just login <- getField $ auth_login msg
  , Just pw    <- getField $ auth_password msg
  = authClient login pw

  | otherwise
  = do putStrLn $ "Incoming (unknown) message: " ++ show msg
       return $ responseErrorMessage Nothing

authClient :: Text -> Text -> IO PB_ServerMessage
authClient login pw = do

  putStrLn $ "Authenticate client: " ++ Text.unpack login

  result <- runQuery $ checkPassword (Text.unpack login) (TE.encodeUtf8 pw)
  case result of
    Right True -> return $ responseOkMessage
    _          -> return $ responseErrorMessage $ Just "Invalid user/password"
