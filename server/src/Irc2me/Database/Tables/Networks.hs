{-# LANGUAGE TemplateHaskell #-}

module Irc2me.Database.Tables.Networks where

import Control.Applicative
import Data.List

-- lens
import Control.Lens hiding (Identity)

-- hdbc
import Database.HDBC

import Irc2me.Database.Helper
import Irc2me.Database.Query
import Irc2me.Database.Tables.Accounts

import Irc2me.Frontend.Messages.Helper   as Messages
import Irc2me.Frontend.Messages.Identity as Messages
import Irc2me.Frontend.Messages.Network  as Messages

newtype NetworkID = NetworkID { _networkId :: Integer }
  deriving (Eq, Ord, Show)

makeLenses ''NetworkID

selectServersToReconnect :: AccountID -> Query [(NetworkID, Server)]
selectServersToReconnect (AccountID acc) = Query
  query
  [toSql acc]
  (convertList $ \(x:r) -> (,) <$> toNetworkId [x] <*> toServer r)
 where
  query = "SELECT n.id, " ++ (qualified "s" serverFields & intercalate ", ") ++
          "  FROM servers_to_reconnect AS s, " ++ networkTable `as` "n" ++
          " WHERE s.network = n.id \
          \   AND n.account = ?"

--------------------------------------------------------------------------------
-- "networks" table

-- converters

networkTable :: String
networkTable = "networks"

networkSELECT :: String
networkSELECT = "SELECT id, name, reconnect, identity FROM networks"

toNetworkId :: Converter NetworkID
toNetworkId [SqlInteger i] = Just $ NetworkID i
toNetworkId _              = Nothing

toNetwork :: Converter Network
toNetwork [SqlInteger i, SqlByteString name, SqlBool reconnect, ident] = Just $

  emptyNetwork &~ do
    Messages.networkId         .= Just (fromIntegral i)
    Messages.networkName       .= Just (name ^. encoded)
    Messages.networkReconnect  .= Just reconnect
    Messages.networkIdentity   .= case ident of
                                    SqlInteger i' ->
                                      Just $ emptyIdentity
                                            & identityId .~ Just (fromIntegral i')
                                    _             -> Nothing

toNetwork _ = Nothing

-- queries

selectNetworks :: AccountID -> Query [Network]
selectNetworks (AccountID a) = Query
  (networkSELECT ++ " WHERE account = ? ORDER BY ord, id")
  [toSql a]
  (convertList toNetwork)

selectNetworkIdentity :: AccountID -> NetworkID -> Query (Maybe Identity)
selectNetworkIdentity (AccountID a) (NetworkID n) = Query
  query
  [toSql a, toSql n]
  (convertOne toIdentity)
 where
  query = "SELECT " ++ intercalate ", " (qualified "i" identityFields) ++
          "  FROM network_identities as i, networks as n \
          \ WHERE n.account = ? \
          \   AND n.id = ? "

-- updates

addNetwork :: AccountID -> String -> Bool -> Update (Maybe Network)
addNetwork (AccountID a) name reconnect = UpdateReturning
  "INSERT INTO networks (account, name, reconnect) VALUES (?, ?, ?) \
  \  RETURNING id, name, reconnect, identity"
  [toSql a, toSql name, toSql reconnect]
  (convertOne toNetwork)

--------------------------------------------------------------------------------
-- "network_servers" table

-- converters

serverFields :: [String]
serverFields = ["address", "port", "use_tls"]

serverTable :: String
serverTable = "network_servers"

serverSELECT :: String
serverSELECT = "SELECT " ++ intercalate ", " serverFields ++ " FROM " ++ serverTable

toServer :: Converter Server
toServer s = case s of
  [SqlByteString a, SqlInteger p, SqlBool ssl] -> Just $
    emptyServer &~ do
      serverHost    .= Just (a ^. encoded)
      serverPort    .= Just (fromIntegral p)
      serverUseTLS  .= Just ssl
  _ -> Nothing

-- queries

selectServers :: Network -> Query [Server]
selectServers netw = Query
  (serverSELECT ++ " WHERE network = ?")
  [toSql $ netw ^. Messages.networkId]
  (convertList toServer)

-- updates

addServer :: Network -> Server -> Update Bool
addServer netw server = Update
  "INSERT INTO network_servers (network, address, port, use_tls) \
   \    VALUES (?, ?, ?, ?)"
  [ toSql $ netw ^. Messages.networkId
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
