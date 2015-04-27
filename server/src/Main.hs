module Main where

import Network (withSocketsDo)

import Irc2me

conf :: ServerConfig
conf = ServerConfig
  { serverTLSAddress    = "0.0.0.0"
  , serverTLSPort       = 6565
  , serverTLSCert       = "cert.pem"
  , serverTLSPrivateKey = "cert.pem"
  , serverTLSCAFile     = Just "ca.pem"
  }

main :: IO ()
main = withSocketsDo $
  runServer conf
