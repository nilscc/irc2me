module Irc2me.Frontend.Streams.System where

import Control.Lens.Operators

import Irc2me.Frontend.Messages

import Irc2me.Frontend.Streams.StreamT
import Irc2me.Frontend.Streams.Helper

systemStream :: ServerResponse
systemStream = choice

  [ do guardSystemMsg SystemMsg_PING
       sendSystemMsg  SystemMsg_PONG

  , do guardSystemMsg SystemMsg_Disconnect
       disconnect Nothing
  ]

 where
  guardSystemMsg msg =
    guardMessageFieldValue clientSystemMsg $ Just msg
  sendSystemMsg  msg =
    return $ emptyServerMessage & serverSystemMsg .~ Just msg
