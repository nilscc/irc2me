{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IRC.TLS where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception

import Crypto.Random

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Network.TLS
import Network.TLS.Extra

import IRC.Types

clientParams :: Params
clientParams = defaultParamsClient
  { pConnectVersion = TLS12
  , pAllowedVersions = [SSL3, TLS10, TLS11, TLS12]
  , pCiphers = ciphersuite_all
  }

establishTLS :: Connection -> IO Connection
establishTLS con@Connection{ con_tls_context = Just _ }  = return con
establishTLS con@Connection{ con_tls_context = Nothing } = do

  -- entropy & random gen
  gen <- cprgCreate `fmap` createEntropyPool :: IO SystemRNG

  -- create TLS context
  ctxt <- contextNewOnHandle (con_handle con)
                             clientParams
                             gen

  handshake ctxt

  -- create incoming buffer
  buff <- newTVarIO BS.empty

  -- receive data in background
  tid <- forkIO $ forever $ handleException $ do
    bs <- recvData ctxt
    atomically $ modifyTVar buff (`BS.append` bs)

  return con{ con_tls_context = Just (ctxt, buff, tid) }
 where
  handleException = handle (\(_ :: IOException) -> return ())

tlsGetLine :: Connection -> IO ByteString
tlsGetLine Connection{ con_tls_context = Just (_, buff, _) } = do
  atomically $ do
    bs <- readTVar buff `orElse` retry
    case B8.span (/= '\r') bs of
      (line, bs1)
        | Just ('\r', bs2)  <- B8.uncons bs1
        , Just ('\n', rest) <- B8.uncons bs2 -> do
          writeTVar buff rest
          return $ line `B8.append` "\r\n"
      _ -> retry
tlsGetLine con = do
  BS.hGetLine (con_handle con)

tlsSend :: Connection -> ByteString -> IO ()
tlsSend con bs = do
  case con_tls_context con of
    Just (ctxt, _, _) -> sendData ctxt (BL.fromStrict bs)
    Nothing           -> BS.hPutStr (con_handle con) bs

-- | Close a TLS session. May fail is handle is already closed!
sendBye :: Connection -> IO ()
sendBye con = do
  case con_tls_context con of
    Nothing -> return ()
    Just (ctxt,_,tid) -> do
      bye ctxt
      killThread tid
