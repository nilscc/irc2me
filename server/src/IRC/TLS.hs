{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module IRC.TLS where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

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
  tid <- forkIO $ forever $ do
    bs <- recvData ctxt
    atomically $ modifyTVar buff (`BS.append` bs)

  return con{ con_tls_context = Just (ctxt, buff, tid) }

tlsGetLine :: Connection -> IO ByteString
tlsGetLine Connection{ con_tls_context = Just (_, buff, _) } = do
  l <- atomically $ do
    bs <- readTVar buff
    case B8.span (/= '\r') bs of
      (line, bs1)
        | Just ('\r', bs2)  <- B8.uncons bs1
        , Just ('\n', rest) <- B8.uncons bs2 -> do
          writeTVar buff rest
          return $ line `B8.append` "\n\r"
      _ -> retry
  return l
tlsGetLine con = do
  BS.hGetLine (con_handle con)

tlsSend :: Connection -> ByteString -> IO ()
tlsSend Connection{ con_tls_context = Just (ctxt, _, _) } bs = do
  sendData ctxt (BL.fromStrict bs)
tlsSend con bs = do
  BS.hPutStr (con_handle con) bs

sendBye :: Connection -> IO ()
sendBye con =
  case con_tls_context con of
    Nothing -> return ()
    Just (ctxt,_,tid) -> do
      bye ctxt
      killThread tid
