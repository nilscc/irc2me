module Data.Serialize.Pipes where

import Control.Monad

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- cereal
import Data.Serialize.Get
import Data.Serialize.Put

-- pipes
import Pipes

getPartialPipe :: Monad m => Get a -> Pipe ByteString a m (Maybe (String, ByteString))
getPartialPipe g = go (runGetPartial g) Nothing
 where

  -- await new chunk
  go f Nothing = do
    chunk <- await
    go f (Just chunk)

  -- use old chunk
  go f (Just chunk) | BS.null chunk = go f Nothing
                    | otherwise = do
    case f chunk of
      Fail err res -> return $ Just (err, res)
      Partial f'   -> go f' Nothing
      Done a res   -> do
        yield a
        -- loop with initial function
        go (runGetPartial g) (Just res)

getPipe :: Monad m => Get a -> Pipe ByteString (Either String a) m ()
getPipe g = forever $ do
  bs <- await
  yield $ runGet g bs

putPipe :: Monad m => (a -> Put) -> Pipe a ByteString m ()
putPipe p = forever $ do
  a <- await
  yield $ runPut (p a)
