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

import           Data.Functor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Network.IRC.ByteString.Parser
import Network.TLS
import Network.TLS.Extra

import IRC.Debug
import IRC.Messages
import IRC.Types

clientParams :: Params
clientParams = defaultParamsClient
  { pConnectVersion = TLS12
  , pAllowedVersions = [SSL3, TLS10, TLS11, TLS12]
  , pCiphers = ciphersuite_all
  }

establishTLS :: Connection -> IO ()
establishTLS con = do
  tls_cont <- getTlsContext con
  case tls_cont of
    Just _ -> return ()
    Nothing -> do
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

      atomically $ writeTVar (con_tls_context con) $ Just (ctxt, buff, tid)
 where
  handleException = handle (\(_ :: IOException) -> return ())

--------------------------------------------------------------------------------
-- TLS initialization

initTLS
  :: Connection
  -> TLSSettings
  -> IO ()

-- no TLS
initTLS con NoTLS = do
  logI con "initTLS" "Plain text"
  sendUserAuth con

-- start with TLS handshake immediately
initTLS con TLS = do
  logI con "initTLS" "TLS"
  establishTLS con
  sendUserAuth con

-- enforce STARTTLS
initTLS con STARTTLS = do
  logI con "initTLS" "STARTTLS"
  sendSTARTTLS con
  tls_succ <- waitForTLS con
  if tls_succ
    then sendUserAuth con
    else closeConnection con -- quit immediately

-- try to find out if server supports TLS via CAP
initTLS con OptionalSTARTTLS = do

  sendCAPLS con
  sendUserAuth con

  cap <- waitForCAP con
  if "tls" `elem` cap then do
    logI con "initTLS" "STARTTLS via CAP"
    sendSTARTTLS con
    -- wait for TLS, fall back to plaintext by returning the old connection on
    -- 'Nothing'
    void $ waitForTLS con
   else
    logI con "initTLS" "STARTTLS via CAP failed, falling back to plain text"

  sendCAPEnd con

waitForTLS :: Connection -> IO Bool
waitForTLS con = do
  mmsg <- receive con
  case mmsg of
    Right msg@(msgCmd -> cmd)

      -- TLS success
      | cmd == "670" -> True <$ establishTLS con

      -- TLS fail
      | cmd == "691" -> return False

      -- ignore NOTICE message, FIXME: add messages to message queue
      | cmd == "NOTICE" -> waitForTLS con

      -- unknown message
      | otherwise -> do
        logE con "waitForTLS" $ "Unexpected message: " ++ show msg ++ " (TLS init failed)"
        return False

    _ -> return False

waitForCAP :: Connection -> IO [ByteString]
waitForCAP con = do
  mmsg <- receive con
  case mmsg of
    Right msg@(msgCmd -> cmd)

      | cmd == "CAP" -> return $ B8.words (msgTrail msg)

      | otherwise -> do
        logE con "waitForTLS" $ "Unexpected message: " ++ show msg
        waitForCAP con

    _ -> return []
