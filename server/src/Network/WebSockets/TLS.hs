module Network.WebSockets.TLS where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Crypto.Random
import qualified Network.Socket as Socket
import Network.TLS as TLS
import Network.WebSockets
import Network.WebSockets.Stream.TLS

runTLSServer
  :: (CPRG rng, TLSParams params)
  => String
  -> Int
  -> params
  -> rng
  -> ServerApp
  -> IO ()
runTLSServer host port params rng app =
  runTLSServerWith host port params rng defaultConnectionOptions app

runTLSServerWith
  :: (CPRG rng, TLSParams params)
  => String
  -> Int
  -> params
  -> rng
  -> ConnectionOptions
  -> ServerApp
  -> IO ()
runTLSServerWith host port params rng opts app = do
  bracket (makeListenSocket host port) Socket.sClose $ \socket -> do
    forever $ mask_ $ do
      (con,_) <- Socket.accept socket
      void $ forkIOWithUnmask $ \unmask ->
        finally (unmask $ runTLSApp con params rng opts app)
                (Socket.sClose con)

runTLSApp
  :: (CPRG rng, HasBackend backend, TLSParams params)
  => backend
  -> params
  -> rng
  -> ConnectionOptions
  -> ServerApp
  -> IO ()
runTLSApp b p r opts app = do
  stream <- makeTLSStream b p r
  pending <- makePendingConnection stream opts
  app pending
