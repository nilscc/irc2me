{-# LANGUAGE LambdaCase #-}

module Database.Tables.Accounts where

import Data.ByteString (ByteString)

import Crypto.Scrypt
import Database.HDBC

import Database.Query

newtype Account = Account { accountId :: Integer }
  deriving (Eq, Show, Ord)

-- converters

accountSELECT :: String
accountSELECT = "SELECT id FROM accounts"

toAccount :: Converter Account
toAccount = \case
  [SqlInteger i] -> Just $ Account i
  _              -> Nothing

toBool :: Converter Bool
toBool = \case
  [SqlBool b] -> Just b
  _           -> Nothing

toByteString :: Converter ByteString
toByteString = \case
  [SqlByteString bs] -> Just bs
  _                  -> Nothing

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

checkPassword :: String -> ByteString -> Query Bool
checkPassword login pw = Query
  "SELECT password FROM accounts WHERE login = ?"
  [toSql login]
  (verify . convertOne toByteString)
 where
  verify = \case
    (Just bs) -> fst $ verifyPass defaultParams (Pass pw) (EncryptedPass bs)
    _         -> False

