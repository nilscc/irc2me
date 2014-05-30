module Database.Tables.Networks where

import Database.HDBC
import Network
import qualified Data.ByteString.Char8 as B8

import Database.Query
import Database.Tables.Accounts

import IRC.Types

--------------------------------------------------------------------------------
-- "networks" table

data Network = Network
  { networkId     :: Integer
  }
  deriving (Eq, Show, Ord)

-- converters

networkSELECT :: String
networkSELECT = "SELECT id FROM networks"

toNetwork :: Converter Network
toNetwork s = case s of
  [SqlInteger i] -> Just $ Network i
  _              -> Nothing

-- queries

selectNetworks :: Account -> Query [Network]
selectNetworks (Account a) = Query
  (networkSELECT ++ " WHERE account = ?")
  [toSql a]
  (convertList toNetwork)

-- updates

addNetwork :: Account -> String -> Update (Maybe Network)
addNetwork (Account a) name = UpdateReturning
  "INSERT INTO networks (account, name) VALUES (?, ?) \
  \  RETURNING id"
  [toSql a, toSql name]
  (convertOne toNetwork)

--------------------------------------------------------------------------------
-- "network_servers" table

-- converters

serverSELECT :: String
serverSELECT = "SELECT address, port, use_ssl FROM network_servers"

toServer :: Converter Server
toServer s = case s of
  [SqlString a, SqlInteger p, SqlBool ssl] -> Just $
    Server { srv_host       = a
           , srv_port       = PortNumber $ fromIntegral p
           , srv_tls        = if ssl then TLS else OptionalSTARTTLS
           , srv_reconnect  = False
           }
  _ -> Nothing

-- queries

selectServers :: Network -> Query [Server]
selectServers (Network i) = Query
  (serverSELECT ++ " WHERE network = ?")
  [toSql i]
  (convertList toServer)

-- updates

addServer :: Network -> Server -> Update Bool
addServer (Network n) server = Update
  "INSERT INTO network_servers (network, address, port, use_ssl) \
   \    VALUES (?, ?, ?, ?)"
  [ toSql n
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
selectChannels (Network i) = Query
  (channelSELECT ++ " WHERE network = ?")
  [toSql i]
  (convertList toChannel)

-- updates

addChannel :: Network -> Channel -> Update Bool
addChannel (Network i) chan = Update
  "INSERT INTO network_channels (network, name) VALUES (?, ?)"
  [toSql i, toSql chan]
  (== 1)
