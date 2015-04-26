module Network.WebSockets.TLS
  ( runTLSServer
  , runTLSServerWith
  , runSimpleTLSServer
  ) where

import Data.Default.Class
import Control.Exception
import Control.Concurrent
import Control.Monad
import Crypto.Random
import Data.X509.File
import qualified Network.Socket as Socket
import Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import Network.WebSockets
import Network.WebSockets.Stream.TLS

-- | Basic server using TLS.
runTLSServer
  :: CPRG rng
  => String         -- ^ Address to bind
  -> Int            -- ^ Port to listen on
  -> ServerParams   -- ^ TLS server parameters
  -> rng            -- ^ Random number generator
  -> ServerApp      -- ^ WebSockets application
  -> IO ()
runTLSServer host port params rng app =
  runTLSServerWith host port params rng defaultConnectionOptions app

-- | A version of 'runTLSServer' with custom 'ConnectionOptions'.
runTLSServerWith
  :: CPRG rng
  => String
  -> Int
  -> ServerParams
  -> rng
  -> ConnectionOptions
  -> ServerApp
  -> IO ()
runTLSServerWith host port params rng opts app = do
  bracket (makeListenSocket host port) Socket.sClose $ \socket -> do
    forever $ mask_ $ do
      (con,_) <- Socket.accept socket
      void $ forkIOWithUnmask $ \unmask ->
        (unmask $ runTLSApp con) `finally` (Socket.sClose con)
 where
  runTLSApp con = do
    stream <- makeTLSStream con params rng
    pending <- makePendingConnection stream opts
    app pending

-- | Run a simple TLS server with default TLS configuration
runSimpleTLSServer
  :: String           -- ^ Address to bind
  -> Int              -- ^ Port to listen on
  -> FilePath         -- ^ Public TLS certificate
  -> FilePath         -- ^ Private TLS key
  -> Maybe FilePath   -- ^ Optional CA certificate
  -> ServerApp
  -> IO ()
runSimpleTLSServer host port cert key opt_ca app = do

  -- try to load certificate + key
  mcred <- credentialLoadX509 cert key

  case mcred of
    Left e -> error e
    Right cred -> do

      -- try to load CA (if any)
      ca <- maybe (return []) readSignedObject opt_ca

      -- load system RNG
      ent <- createEntropyPool
      let rng = cprgCreate ent :: SystemRNG

      -- TLS server parameters
      let params = def
            { serverCACertificates = ca
            , serverShared = def
                { sharedCredentials = Credentials [cred]
                }
            , serverSupported = def { supportedCiphers  = TLS.ciphersuite_strong }
            }

      runTLSServer host port params rng app
