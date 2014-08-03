module Server.Streams.System where

import Control.Monad
import Control.Lens.Operators
import Data.ProtocolBuffers

import ProtoBuf.Helper
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server
import ProtoBuf.Messages.SystemMsg

import Server.Streams
import Server.Response

systemStream :: ServerResponse
systemStream = do

  msg <- getClientMessage

  case client_system_msg msg ^. field of

    Just PB_SystemMsg_PING -> systemMsg PB_SystemMsg_PONG

    Just PB_SystemMsg_Disconnect ->
      throwS "systemStream" "Disconnected."

    _ -> mzero

 where
  systemMsg msg = return $ emptyServerMessage & server_system_msg .~~ Just msg
