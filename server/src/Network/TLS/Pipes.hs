{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.TLS.Pipes where

import qualified Data.ByteString.Char8 as B8

import Control.Exception
import Control.Monad.Except

import Data.ByteString

import System.IO.Error

-- tls package
import Network.TLS

-- pipes
import Pipes

type TLSProducer m = Producer ByteString m (Maybe (Either TLSException IOException))

fromTLS
  :: ( MonadIO m
     , HasBackend backend, TLSParams params
     )
  => backend
  -> params
  -> m (Either (Either TLSException IOException) (TLSProducer m, Context))
fromTLS h params = do

  mctxt <- runExceptT $ mkSafe $ do

    -- in IO monad
    ctxt <- contextNew h params
    handshake ctxt
    return ctxt

  case mctxt of

    -- directly return exception in producer
    Left exc -> return $ Left exc

    -- run loop on context
    Right ctxt -> return $ toResult ctxt $ fix $ \loop -> do
      mbs <- mkSafe $ handleEOF $ recvData ctxt
      withJust mbs $ \bs -> do
        unless (B8.null bs) $
          lift $ yield bs
        loop

 where

  toResult ctxt p = Right ( either Just (const Nothing) <$> runExceptT p
                          , ctxt)

  mkSafe :: (MonadIO m, MonadError (Either TLSException IOException) m) => IO a -> m a
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
