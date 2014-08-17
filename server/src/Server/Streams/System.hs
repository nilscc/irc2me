module Server.Streams.System where

import Control.Lens.Operators

import ProtoBuf.Helper
import ProtoBuf.Messages.Client
import ProtoBuf.Messages.Server
import ProtoBuf.Messages.SystemMsg

import Server.Streams
import Server.Response

systemStream :: ServerResponse
systemStream = choice

  [ do guardSystemMsg PB_SystemMsg_PING
       sendSystemMsg  PB_SystemMsg_PONG

  , do guardSystemMsg PB_SystemMsg_Disconnect
       disconnect Nothing
  ]

 where
  guardSystemMsg msg = guardMessageFieldValue client_system_msg $ Just msg
  sendSystemMsg  msg = return $ emptyServerMessage & server_system_msg .~~ Just msg
