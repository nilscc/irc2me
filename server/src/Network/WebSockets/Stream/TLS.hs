module Network.WebSockets.Stream.TLS where

import Control.Monad.Trans
import qualified Data.ByteString as BS
import Network.TLS as TLS
import Network.WebSockets.Stream

makeTLSStream
  :: (MonadIO m, HasBackend backend, TLSParams params)
  => backend
  -> params
  -> m Stream
makeTLSStream b p = liftIO $ do
  ctxt <- TLS.contextNew b p
  handshake ctxt
  makeStream (receive ctxt) (send ctxt)
 where
  receive c = do
    bs <- TLS.recvData c
    return $ if BS.null bs then Nothing else Just bs
  send c Nothing = TLS.bye c
  send c (Just bs) = do
    TLS.sendData c bs
