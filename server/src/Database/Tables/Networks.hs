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
  , networkName   :: String
  }
  deriving (Eq, Show, Ord)

-- converters

networkSELECT :: String
networkSELECT = "SELECT id, name FROM networks"

toNetwork :: Converter Network
toNetwork s = case s of
  [SqlInteger i, SqlString n] -> Just $ Network i n
  _                           -> Nothing

-- queries

selectNetworks :: Account -> Query [Network]
selectNetworks (Account a) = Query
  (networkSELECT ++ " WHERE account = ?")
  [toSql a]
  (convertList toNetwork)


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
selectServers (Network i _) = Query
  (serverSELECT ++ " WHERE network = ?")
  [toSql i]
  (convertList toServer)

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
selectChannels (Network i _) = Query
  (channelSELECT ++ " WHERE network = ?")
  [toSql i]
  (convertList toChannel)

addChannel :: Network -> Channel -> Update Bool
addChannel (Network i _) chan = Update
  "INSERT INTO network_channels (network, name) VALUES (?, ?)"
  [toSql i, toSql chan]
  (== 1)
