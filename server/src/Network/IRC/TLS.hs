{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.TLS where

import Data.Default.Class
import Data.X509.Validation
import System.X509

import Network.TLS
import Network.TLS.Extra

initClientParams :: HostName -> IO ClientParams
initClientParams hn = do
  cstore <- getSystemCertificateStore
  return (defaultParamsClient hn "irc2me-server")
    { clientSupported = def { supportedCiphers = ciphersuite_strong }
    , clientShared    = def { sharedCAStore = cstore }
    }
