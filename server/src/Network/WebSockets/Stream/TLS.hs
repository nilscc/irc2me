module Network.WebSockets.Stream.TLS where

import Control.Monad.Trans
import Crypto.Random (CPRG)
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as B8
import Network.TLS as TLS
import Network.WebSockets.Stream

makeTLSStream
  :: (MonadIO m, CPRG rng, HasBackend backend, TLSParams params)
  => backend
  -> params
  -> rng
  -> m Stream
makeTLSStream b p r = liftIO $ do
  ctxt <- TLS.contextNew b p r
  handshake ctxt
  makeStream (receive ctxt) (send ctxt)
 where
  receive c = do
    bs <- TLS.recvData c
    return $ if BS.null bs then Nothing else Just bs
  send c Nothing = TLS.bye c
  send c (Just bs) = do
    TLS.sendData c bs
