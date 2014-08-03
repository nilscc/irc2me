module Server.Response where

import Control.Monad.Reader
import Data.Monoid

import Database.Tables.Accounts
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server
import Server.Streams

data ServerReaderState = ServerReaderState
  { connectionAccount :: Maybe Account
  , clientMessage     :: PB_ClientMessage
  }

type ServerResponseT = StreamT (First String) (ReaderT ServerReaderState IO)
type ServerResponse  = ServerResponseT PB_ServerMessage

getServerResponse
  :: ServerReaderState -> ServerResponse -> Stream PB_ServerMessage
getServerResponse state resp =
  liftMonadTransformer (runReaderT `flip` state) resp

withAccount :: (Account -> ServerResponse) -> ServerResponse
withAccount f = do
  macc <- lift $ asks connectionAccount
  case macc of
    Nothing -> throwS "withAccount" "Login required"
    Just acc -> f acc

getClientMessage :: ServerResponseT PB_ClientMessage
getClientMessage = lift $ asks clientMessage
