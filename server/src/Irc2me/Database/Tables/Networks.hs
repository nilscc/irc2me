module Irc2me.Database.Tables.Networks where

import Control.Applicative
import Data.List

-- lens
import Control.Lens

-- hdbc
import Database.HDBC

import Irc2me.Database.Helper
import Irc2me.Database.Query
import Irc2me.Database.Tables.Accounts

import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages.IrcIdentity
import Irc2me.ProtoBuf.Messages.IrcNetwork

selectServersToReconnect :: AccountID -> Query [(NetworkID, IrcServer)]
selectServersToReconnect (AccountID acc) = Query
  query
  [toSql acc]
  (convertList $ \(x:r) -> (,) <$> toNetworkId [x] <*> toIrcServer r)
 where
  query = "SELECT n.id, " ++ (qualified "s" serverFields & intercalate ", ") ++
          "  FROM servers_to_reconnect AS s, " ++ networkTable `as` "n" ++
          " WHERE s.network = n.id \
          \   AND n.account = ?"

--------------------------------------------------------------------------------
-- "networks" table

newtype NetworkID = NetworkID { _networkId :: Integer }
  deriving (Eq, Ord, Show)

-- converters

networkTable :: String
networkTable = "networks"

networkSELECT :: String
networkSELECT = "SELECT id, name, reconnect, identity FROM networks"

toNetworkId :: Converter NetworkID
toNetworkId [SqlInteger i] = Just $ NetworkID i
toNetworkId _              = Nothing

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

selectNetworks :: AccountID -> Query [IrcNetwork]
selectNetworks (AccountID a) = Query
  (networkSELECT ++ " WHERE account = ? ORDER BY ord, id")
  [toSql a]
  (convertList toIrcNetwork)

selectNetworkIdentity :: AccountID -> NetworkID -> Query (Maybe IrcIdentity)
selectNetworkIdentity (AccountID a) (NetworkID n) = Query
  query
  [toSql a, toSql n]
  (convertOne toIrcIdentity)
 where
  query = "SELECT " ++ intercalate ", " (qualified "i" identityFields) ++
          "  FROM network_identities as i, networks as n \
          \ WHERE n.account = ? \
          \   AND n.id = ? "

-- updates

addNetwork :: AccountID -> String -> Bool -> Update (Maybe IrcNetwork)
addNetwork (AccountID a) name reconnect = UpdateReturning
  "INSERT INTO networks (account, name, reconnect) VALUES (?, ?, ?) \
  \  RETURNING id, name, reconnect, identity"
  [toSql a, toSql name, toSql reconnect]
  (convertOne toIrcNetwork)

--------------------------------------------------------------------------------
-- "network_servers" table

-- converters

serverFields :: [String]
serverFields = ["address", "port", "use_tls"]

serverTable :: String
serverTable = "network_servers"

serverSELECT :: String
serverSELECT = "SELECT " ++ intercalate ", " serverFields ++ " FROM " ++ serverTable

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
