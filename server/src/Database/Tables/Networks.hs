module Database.Tables.Networks where

import Database.HDBC
import Network
import qualified Data.ByteString.Char8 as B8

import Database.Query
import Database.Tables.Accounts

import IRC.Types

--------------------------------------------------------------------------------
-- "networks" table

-- converters

networkSELECT :: String
networkSELECT = "SELECT id, name, reconnect, identity FROM networks"

toNetwork :: Converter Network
toNetwork [SqlInteger i, SqlByteString name, SqlBool reconnect, ident ] = Just $
  Network { netw_id        = i
          , netw_name      = B8.unpack name
          , netw_reconnect = reconnect
          , netw_identity  = case ident of
                               SqlInteger i' -> Just i'
                               _             -> Nothing
          }
toNetwork _ = Nothing

-- queries

selectNetworks :: Account -> Query [Network]
selectNetworks (Account a) = Query
  (networkSELECT ++ " WHERE account = ?")
  [toSql a]
  (convertList toNetwork)

-- updates

addNetwork :: Account -> String -> Bool -> Update (Maybe Network)
addNetwork (Account a) name reconnect = UpdateReturning
  "INSERT INTO networks (account, name, reconnect) VALUES (?, ?, ?) \
  \  RETURNING id, name, reconnect, identity"
  [toSql a, toSql name, toSql reconnect]
  (convertOne toNetwork)

--------------------------------------------------------------------------------
-- "network_servers" table

-- converters

serverSELECT :: String
serverSELECT = "SELECT address, port, use_tls FROM network_servers"

toServer :: Converter Server
toServer s = case s of
  [SqlByteString a, SqlInteger p, SqlBool ssl] -> Just $
    Server { srv_host       = B8.unpack a
           , srv_port       = PortNumber $ fromIntegral p
           , srv_tls        = if ssl then TLS else OptionalSTARTTLS
           }
  _ -> Nothing

-- queries

selectServers :: Network -> Query [Server]
selectServers netw = Query
  (serverSELECT ++ " WHERE network = ?")
  [toSql $ netw_id netw]
  (convertList toServer)

-- updates

addServer :: Network -> Server -> Update Bool
addServer netw server = Update
  "INSERT INTO network_servers (network, address, port, use_tls) \
   \    VALUES (?, ?, ?, ?)"
  [ toSql $ netw_id netw
  , toSql $ srv_host server
  , toSql $ fromPortID $ srv_port server
  , toSql $ srv_tls server == TLS
  ]
  (== 1)
 where
  fromPortID (PortNumber pn) = fromIntegral pn :: Integer
  fromPortID _               = 6667

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
  [toSql (netw_id netw)]
  (convertList toChannel)

-- updates

addChannel :: Network -> Channel -> Update Bool
addChannel netw chan = Update
  "INSERT INTO network_channels (network, name) VALUES (?, ?)"
  [toSql (netw_id netw), toSql chan]
  (== 1)
