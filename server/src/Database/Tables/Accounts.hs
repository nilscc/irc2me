{-# LANGUAGE LambdaCase #-}

module Database.Tables.Accounts where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Crypto.Scrypt
import Database.HDBC

import Database.Query
import IRC.Types

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
identitySELECT = "SELECT username, realname, nick, nick_alt FROM account_identities"

toIdentity :: Converter Identity
toIdentity s = case s of
  [SqlByteString u, SqlByteString r, SqlByteString n, na] -> Just $
    Identity { usr_name = u
             , usr_realname = r
             , usr_nick = n
             , usr_nick_alt = maybe [] (map B8.pack) $ arrayUnpack na
             }
  _ -> Nothing

-- queries

selectIdentities :: Account -> Query [Identity]
selectIdentities (Account a) = Query
  (identitySELECT ++ " WHERE account = ?")
  [toSql a]
  (convertList toIdentity)

-- updates

addIdentity :: Account -> Identity -> Update Bool
addIdentity (Account a) usr = Update
  "INSERT INTO account_identities (account, username, realname, nick, nick_alt) \
  \     VALUES                    (?      , ?       , ?       , ?   , ?)"
  [ toSql a
  , toSql $ usr_name usr
  , toSql $ usr_realname usr
  , toSql $ usr_nick usr
  , arrayPack $ map B8.unpack $ usr_nick_alt usr
  ]
  (== 1)
