module Irc2me.Database.Tables.IRC.Networks where

import Control.Applicative
import Control.Lens hiding (Identity)
import Data.List
import qualified Data.ByteString.Char8 as B8

import Database.HDBC

import Irc2me.Database.Query
import Irc2me.Database.Helper
import Irc2me.Database.Tables.Accounts -- (ID, AccountID, toID)

import Irc2me.Backends.IRC.Helper (encoded)
import Irc2me.Backends.IRC.Types (Network(..), NetworkID, Server(..), Identity(..))
-- import qualified Irc2me.Backends.IRC.Types as IRC

--------------------------------------------------------------------------------
-- Networks

networkTable :: String
networkTable = "networks"

-- | Network converter
toNetwork :: Converter Network
toNetwork [SqlInteger i, SqlByteString name, SqlBool reconnect, ident] = Just $

  Network { _networkId        = i
          , _networkName      = (name ^. encoded)
          , _networkReconnect = reconnect
          , _networkIdentity  = toID [ident]
          , _networkServers   = []
          }

toNetwork _ = Nothing

--------------------------------------------------------------------------------
-- Identities

identityFields :: [String]
identityFields = ["id", "username", "realname", "nick", "nick_alt"]

-- identityTable :: String
-- identityTable = "account_identities"

-- identitySELECT :: String
-- identitySELECT = "SELECT " ++ intercalate ", " identityFields ++ " FROM " ++ identityTable

-- converters

toIdentity :: Converter Identity
toIdentity s = case s of
  [SqlInteger i, SqlByteString u, SqlByteString r, SqlByteString n, _na] -> Just $
    Identity { _identityId       = i
             , _identityName     = u ^. encoded
             , _identityRealname = r ^. encoded
             , _identityNick     = n ^. encoded
             --, _identityNickAlt  = maybe [] (map (^. re _Text)) (arrayUnpack na)
             }
  _ -> Nothing

-- queries

selectNetworkIdentity :: AccountID -> NetworkID -> Query (Maybe Identity)
selectNetworkIdentity a n = Query
  query
  [toSql a, toSql n]
  (convertOne toIdentity)
 where
  query = "SELECT " ++ intercalate ", " (qualified "i" identityFields) ++
          "  FROM network_identities as i, networks as n \
          \ WHERE n.account = ? \
          \   AND n.id = ? "

--------------------------------------------------------------------------------
-- Servers

serverFields :: [String]
serverFields = ["address", "port", "use_tls"]

-- | Server converter
toServer :: Converter Server
toServer s = case s of
  [SqlByteString a, SqlInteger p, SqlBool ssl] -> Just $
    Server { _serverHost    = B8.unpack a
           , _serverPort    = fromIntegral p
           , _serverUseTLS  = ssl
           }
  _ -> Nothing

selectServersToReconnect :: AccountID -> Query [(NetworkID, Server)]
selectServersToReconnect (acc) = Query
  query
  [toSql acc]
  (convertList $ \(x:r) -> (,) <$> toID [x] <*> toServer r)
 where
  query = "SELECT n.id, " ++ (qualified "s" serverFields & intercalate ", ") ++
          "  FROM servers_to_reconnect AS s, " ++ networkTable `as` "n" ++
          " WHERE s.network = n.id \
          \   AND n.account = ?"
