{-# LANGUAGE LambdaCase #-}

module Database.Tables.Accounts where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Crypto.Scrypt
import Database.HDBC

import Database.Query
import IRC.Types

-- ID type

toID :: Converter ID
toID [SqlInteger i] = Just i
toID _              = Nothing

--
-- Accounts
--

newtype Account = Account { accountId :: Integer }
  deriving (Eq, Show, Ord)

-- converters

accountSELECT :: String
accountSELECT = "SELECT id FROM accounts"

toAccount :: Converter Account
toAccount = \case
  [SqlInteger i] -> Just $ Account i
  _              -> Nothing

-- queries

selectAccounts :: Query [Account]
selectAccounts = Query
  accountSELECT
  []
  (convertList toAccount)

selectAccountByLogin :: String -> Query (Maybe Account)
selectAccountByLogin login = Query
  (accountSELECT ++ " WHERE login = ?")
  [toSql login]
  (convertOne toAccount)

checkPassword :: Account -> ByteString -> Query Bool
checkPassword (Account acc) pw = Query
  "SELECT password FROM accounts WHERE id = ?"
  [toSql acc]
  (verify . convertOne fromBytea)
 where
  verify = \case
    (Just bs) -> fst $ verifyPass defaultParams (Pass pw) (EncryptedPass bs)
    _         -> False

-- updates

mkEncrypted :: ByteString -> IO EncryptedPass
mkEncrypted pw = encryptPassIO defaultParams (Pass pw)

addAccount
  :: String           -- ^ Login name
  -> EncryptedPass    -- ^ Password
  -> Update (Maybe Account)
addAccount login encrypted = UpdateReturning
  "INSERT INTO accounts (login, password) VALUES (?, ?) \
  \  RETURNING id"
  [toSql login, byteaPack (getEncryptedPass encrypted)]
  (convertOne toAccount)

--------------------------------------------------------------------------------
-- "account_identities" table

-- converters

identitySELECT :: String
identitySELECT = "SELECT id, username, realname, nick, nick_alt FROM account_identities"

toIdentity :: Converter Identity
toIdentity s = case s of
  [SqlInteger i, SqlByteString u, SqlByteString r, SqlByteString n, na] -> Just $
    Identity { ident_id = i
             , ident_name = u
             , ident_realname = r
             , ident_nick = n
             , ident_nick_alt = maybe [] (map B8.pack) $ arrayUnpack na
             }
  _ -> Nothing

-- queries

selectIdentities :: Account -> Query [Identity]
selectIdentities (Account a) = Query
  (identitySELECT ++ " WHERE account = ? ORDER BY nick")
  [toSql a]
  (convertList toIdentity)

-- updates

addIdentity :: Account -> Identity -> Update (Maybe ID)
addIdentity (Account a) usr = UpdateReturning
  "INSERT INTO account_identities (account, username, realname, nick, nick_alt) \
  \     VALUES                    (?      , ?       , ?       , ?   , ?)        \
  \  RETURNING id"
  [ toSql a
  , toSql $ ident_name usr
  , toSql $ ident_realname usr
  , toSql $ ident_nick usr
  , arrayPack $ map B8.unpack $ ident_nick_alt usr
  ]
  (convertOne toID)

deleteIdentity :: Account -> ID -> Update Bool
deleteIdentity (Account a) i = Update
  "DELETE FROM account_identities WHERE account = ? AND id = ?"
  [ toSql a
  , toSql i
  ]
  (== 1)

setIdentity :: Account -> Identity -> Update Bool
setIdentity (Account a) ident = Update
  "UPDATE account_identities \
  \   SET username = ?, realname = ?, nick = ?, nick_alt = ?  \
  \ WHERE account = ? AND id = ?"
  [ toSql $ ident_name     ident
  , toSql $ ident_realname ident
  , toSql $ ident_nick     ident
  , arrayPack $ map B8.unpack $ ident_nick_alt ident
  , toSql $ a
  , toSql $ ident_id       ident
  ]
  (== 1)
