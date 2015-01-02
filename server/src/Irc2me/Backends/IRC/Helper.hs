{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.Backends.IRC.Helper where

import Control.Monad.Trans

import Data.Time
import Data.Map (Map)

-- lens
import Control.Lens
--import Data.Text.Lens

-- irc-bytestring
import Network.IRC.ByteString.Parser as IRC

-- local
import Control.Concurrent.Broadcast

import Network.IRC.Connection
import Irc2me.Backends.IRC.NetworkState
import Irc2me.Database.Tables.Accounts as DB
import Irc2me.Database.Tables.Networks as DB
import Irc2me.Frontend.Messages as Message

type IrcConnections = Map AccountID (Map NetworkID NetworkConnection)

type NetworkBroadcast = (Broadcast ServerMessage)

data NetworkConnection = NetworkConnection
  { _networkBroadcast  :: NetworkBroadcast
  , _networkConnection :: Connection IO
  , _networkState      :: NetworkState
  }

makeLenses ''NetworkConnection

rebroadcast :: MonadIO m => NetworkConnection -> ChatMessage -> m ()
rebroadcast nc cm

  | Just PRIVMSG <- cm ^. messageType
  , (to':_)      <- cm ^. messageParams
  = do

    let NetworkID nid = nc ^. networkState . nsNetworkID

    ident <- getNetworkIdentitiy $ nc ^. networkState
    let usr = emptyUser &~ do
                Message.userNick .= (ident ^. identityNick . non "")
                Message.userName .= (ident ^. identityName)

    sendBroadcast (nc ^. networkBroadcast) $ emptyServerMessage &
      serverNetworks .~ [ emptyNetwork &~ do
        Message.networkId .= Just (fromIntegral nid)
        networkChannels   .= [ emptyChannel &~ do
            channelName     .= Just to'
            channelMessages .= [ cm &~ do
                messageFromUser .= Just usr
              ]
          ]
      ]

  | otherwise
  = return ()

------------------------------------------------------------------------------
-- Testing

testFormat :: (UTCTime, IRCMsg) -> String
testFormat (t, msg) = "[" ++ show t ++ "] " ++ show msg

{-
  let time = show t -- formatTime defaultTimeLocale "%T" t

      cmd = (    msg ^? messageType      . _Just . re _Show
             <|> msg ^? messageTypeOther . _Just . _Text
            ) ^. non "?"

      who = (    msg ^? messageFromUser   . _Just . userNick . _Text
             <|> msg ^? messageFromServer . _Just . _Text
            ) ^. non "-"

      par = params ^.. traversed . _Text & intercalate ", "

      cnt = (msg ^? messageContent . _Just . _Text) ^. non ""

  in "["  ++ time ++ "]"
  ++ " "  ++ cmd
  ++ " <" ++ who ++ ">"
  ++ " [" ++ par ++ "]"
  ++ " "  ++ cnt
-}
