module Irc2me.Types
  ( ServerConfig (..)
  , ID, AccountID
  , module Irc2me.Types.Message
  ) where

import Irc2me.Types.Message

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

