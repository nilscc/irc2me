{-# LANGUAGE ScopedTypeVariables #-}

module Network.TLS.Pipes where

import Control.Applicative
import Control.Exception
import Control.Monad.Except

import Data.ByteString

import System.IO.Error

-- crypto
import Crypto.Random

-- tls package
import Network.TLS

-- pipes
import Pipes

receiveTLS
  :: ( MonadIO m
     , HasBackend backend, TLSParams params
     )
  => backend
  -> params
  -> Producer ByteString m (Maybe (Either TLSException IOException))
receiveTLS h params = fmap (either Just (const Nothing)) . runExceptT $ do

  ctxt <- mkSafe $ do
    -- in IO monad
    rng  <- cprgCreate <$> createEntropyPool :: IO SystemRNG
    ctxt <- contextNew h params rng
    handshake ctxt
    return ctxt

  fix $ \loop -> do
    mbs <- mkSafe $ handleEOF $ recvData ctxt
    withJust mbs $ \bs -> do
      lift $ yield bs
      loop

 where

  mkSafe io = do
    r <- liftIO $ handle (\(e :: TLSException) -> return (Left (Left e)))
                . handle (\(e :: IOException ) -> return (Left (Right e)))
                $ Right <$> io
    case r of
      Left  e -> throwError e
      Right a -> return a

  handleEOF = handle eofExc . fmap Just

  eofExc (e :: IOException ) = if isEOFError e then return Nothing else throw e

  withJust ma m = maybe (return ()) m ma
