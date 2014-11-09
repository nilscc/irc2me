module Irc2me.ProtoBuf.Connection where

import Control.Applicative

import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Network.TLS as TLS

import System.IO

type Chunks = [ByteString]

class ClientConnection c where
  sendToClient :: c -> ByteString -> IO ()
  incomingChunks :: c -> IO Chunks

instance ClientConnection TLS.Context where
  sendToClient tls = sendData tls . BL.fromStrict
  incomingChunks tls = sequence (repeat $ recvData tls)

instance ClientConnection Handle where
  sendToClient h = B8.hPutStrLn h
  incomingChunks h = BL.toChunks <$> BL.hGetContents h
