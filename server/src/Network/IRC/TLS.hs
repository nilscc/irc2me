{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.TLS where

import Data.List
import Data.Default.Class

-- tls & related
import Data.X509
import Data.X509.CertificateStore
import Data.X509.Validation
import System.X509

-- local
import Network.TLS
import Network.TLS.Extra

certificationErrorsToIgnore :: [FailedReason]
certificationErrorsToIgnore =
  [ UnknownCA
  ]

type CertValidationFunction =
     CertificateStore
  -> ValidationCache
  -> ServiceID
  -> CertificateChain
  -> IO [FailedReason]

validateButIgnore :: CertValidationFunction
validateButIgnore c v s ch = do
  err <- validateDefault c v s ch
  return $ err \\ certificationErrorsToIgnore

initClientParams :: HostName -> IO ClientParams
initClientParams hn = do
  cstore <- getSystemCertificateStore
  return (defaultParamsClient hn "irc2me-server")
    { clientSupported = def { supportedCiphers = ciphersuite_strong }
    , clientShared    = def { sharedCAStore = cstore }
    , clientHooks     = def { onServerCertificate = validateButIgnore }
    }
