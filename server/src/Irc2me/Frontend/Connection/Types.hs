{-# LANGUAGE TemplateHaskell #-}

module Irc2me.Frontend.Connection.Types where

import Control.Applicative
import Control.Concurrent

import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Network.TLS as TLS

import System.IO

-- lens
import Control.Lens

-- local
import Irc2me.Frontend.Messages.Server

type Chunks = [ByteString]

class IsClientConnection c where
  sendToClient :: c -> ByteString -> IO ()
  incomingChunks :: c -> IO Chunks

instance IsClientConnection TLS.Context where
  sendToClient tls = sendData tls . BL.fromStrict
  incomingChunks tls = sequence (repeat $ recvData tls)

instance IsClientConnection Handle where
  sendToClient h = B8.hPutStr h
  incomingChunks h = BL.toChunks <$> BL.hGetContents h

data ClientConnection = ClientConnection
  { _ccThreadId :: ThreadId
  , _ccSend     :: ServerMessage -> IO ()
  }

makeLenses ''ClientConnection
