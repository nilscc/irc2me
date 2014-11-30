{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.TLS where

import Data.List
import Data.Default.Class
import Data.X509
import Data.X509.CertificateStore
import Data.X509.Validation

import Network.TLS
import Network.TLS.Extra

certificationErrorsToIgnore :: [FailedReason]
certificationErrorsToIgnore =
  [ UnknownCA
  , NameMismatch "irc.freenode.org"
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

clientParams :: HostName -> ClientParams
clientParams hn = (defaultParamsClient hn "irc2me-server")
  { clientSupported = def { supportedCiphers = ciphersuite_strong }
  , clientHooks     = def { onServerCertificate = validateButIgnore }
  }
