module Irc2me.Types where

type ID = Integer
type AccountID = ID

--------------------------------------------------------------------------------
-- Configurations

data ServerConfig = ServerConfig
  { serverTLSAddress      :: String
  , serverTLSPort         :: Int
  , serverTLSCert         :: FilePath
  , serverTLSPrivateKey   :: FilePath
  , serverTLSCAFile       :: Maybe FilePath
  }
  deriving (Eq, Ord, Show)

