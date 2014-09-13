{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Clients where

import Control.Monad
import Control.Monad.Except

import Data.ProtocolBuffers
import Data.ProtocolBuffers.Internal

-- bytestring
import Data.ByteString (ByteString)

-- pipes
import Pipes
import Data.Serialize.Pipes

decodeMsg :: (Monad m, MonadError (String, Maybe ByteString) m, Decode msg)
          => Pipe ByteString msg m ()
decodeMsg = prefixedbs >-> decodemsg
 where
  prefixedbs = do
    r <- getPartialPipe getVarintPrefixedBS
    case r of
      Nothing -> prefixedbs -- should never happen, hmmm
      Just (err, res) -> do
        throwError (err, Just res)

  decodemsg = getPipe decodeMessage >-> eitherThrowOrYield

  eitherThrowOrYield = forever $ do
    ea <- await
    case ea of
      Left err -> throwError (err, Nothing)
      Right a  -> yield a
