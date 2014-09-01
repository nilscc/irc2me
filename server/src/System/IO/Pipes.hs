{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Pipes where

import Control.Applicative
import Control.Exception
import Control.Monad.Except

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import System.IO
import System.IO.Error

-- pipes
import Pipes

-- | 'ByteString' producer from a handle. Maximum blocksize is set to 4096.
fromHandle
  :: MonadIO m
  => Handle
  -> Producer ByteString m (Maybe IOException)
fromHandle = fromHandle' 4096

fromHandle'
  :: MonadIO m
  => Int            -- ^ Maximum number of bytes per block.
  -> Handle
  -> Producer ByteString m (Maybe IOException)
fromHandle' blocksize h = fmap (either Just (const Nothing)) . runExceptT $ do

  fix $ \loop -> do
    mbs <- mkSafe $ handleEOF $ BS.hGetSome h blocksize
    withJust mbs $ \bs -> do
      unless (BS.null bs) $
        lift $ yield bs
      loop

 where
  mkSafe io = do
    r <- liftIO $ handle (\(e :: IOException) -> return (Left e))
                $ Right <$> io
    case r of
      Left e  -> throwError e
      Right a -> return a

  handleEOF = handle eofExc . fmap Just

  eofExc (e :: IOException) = if isEOFError e then return Nothing else throw e

  withJust ma m = maybe (return ()) m ma
