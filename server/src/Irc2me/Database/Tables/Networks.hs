module Irc2me.Database.Tables.Networks where

-- lens
import Control.Lens

-- hdbc
import Database.HDBC

import Irc2me.Database.Query
import Irc2me.Database.Tables.Accounts

import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages.Network


--------------------------------------------------------------------------------
-- "networks" table

-- converters

networkSELECT :: String
networkSELECT = "SELECT id, name, reconnect, identity FROM networks"

toIrcNetwork :: Converter IrcNetwork
toIrcNetwork [SqlInteger i, SqlByteString name, SqlBool reconnect, ident] = Just $

  emptyIrcNetwork &~ do
    networkId         .= Just (fromIntegral i)
    networkName       .= Just (name ^. encoded)
    networkReconnect  .= Just reconnect
    networkIdentity   .= case ident of
                           SqlInteger i' -> Just (fromIntegral i')
                           _             -> Nothing

toIrcNetwork _ = Nothing

-- queries

selectNetworks :: Account -> Query [IrcNetwork]
selectNetworks (Account a) = Query
  (networkSELECT ++ " WHERE account = ? ORDER BY ord, id")
  [toSql a]
  (convertList toIrcNetwork)

-- updates

addNetwork :: Account -> String -> Bool -> Update (Maybe IrcNetwork)
addNetwork (Account a) name reconnect = UpdateReturning
  "INSERT INTO networks (account, name, reconnect) VALUES (?, ?, ?) \
  \  RETURNING id, name, reconnect, identity"
  [toSql a, toSql name, toSql reconnect]
  (convertOne toIrcNetwork)

--------------------------------------------------------------------------------
-- "network_servers" table

-- converters

serverSELECT :: String
serverSELECT = "SELECT address, port, use_tls FROM network_servers"

toIrcServer :: Converter IrcServer
toIrcServer s = case s of
  [SqlByteString a, SqlInteger p, SqlBool ssl] -> Just $
    emptyIrcServer &~ do
      serverHost    .= Just (a ^. encoded)
      serverPort    .= Just (fromIntegral p)
      serverUseTLS  .= Just ssl
  _ -> Nothing

-- queries

selectIrcServers :: IrcNetwork -> Query [IrcServer]
selectIrcServers netw = Query
  (serverSELECT ++ " WHERE network = ?")
  [toSql $ netw ^. networkId]
  (convertList toIrcServer)

-- updates

addIrcServer :: IrcNetwork -> IrcServer -> Update Bool
addIrcServer netw server = Update
  "INSERT INTO network_servers (network, address, port, use_tls) \
   \    VALUES (?, ?, ?, ?)"
  [ toSql $ netw ^. networkId
  , toSql $ server ^. serverHost
  , toSql $ server ^. serverPort
  , toSql $ server ^. serverUseTLS
  ]
  (== 1)

{-
--------------------------------------------------------------------------------
-- "network_channels" table

-- converters

channelSELECT :: String
channelSELECT = "SELECT name FROM network_channels"

toChannel :: Converter Channel
toChannel s = case s of
  [SqlString c] -> Just $ B8.pack c
  _             -> Nothing

-- queries

selectChannels :: Network -> Query [Channel]
selectChannels netw = Query
  (channelSELECT ++ " WHERE network = ?")
  [toSql (netw ^. netw_id)]
  (convertList toChannel)

-- updates

addChannel :: Network -> Channel -> Update Bool
addChannel netw chan = Update
  "INSERT INTO network_channels (network, name) VALUES (?, ?)"
  [toSql (netw ^. netw_id), toSql chan]
  (== 1)
-}
