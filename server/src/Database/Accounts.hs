module Database.Accounts where

import Database.HDBC

import Database.Query

newtype Account = Account { accountId :: Integer }
  deriving (Eq, Show, Ord)

-- converters

toAccount :: Converter Account
toAccount [SqlInteger i] = Just $ Account i
toAccount _              = Nothing

-- queries

selectAccounts :: Query [Account]
selectAccounts = Query
  "SELECT id FROM accounts"
  []
  (convertList toAccount)

selectAccountByLogin :: String -> Query (Maybe Account)
selectAccountByLogin login = Query
  "SELECT id FROM accounts WHERE login = ?"
  [SqlString login]
  (convertOne toAccount)
