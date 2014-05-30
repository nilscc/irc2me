module Database.Accounts where

import Data.Maybe
import Database.HDBC

import Database.Query

newtype Account = Account { accountId :: Integer }
  deriving (Eq, Show, Ord)

-- conversion helpers

type Converter a = [SqlValue] -> Maybe a

one :: Converter a -> [[SqlValue]] -> Maybe a
one f = listToMaybe . list f

list :: Converter a -> [[SqlValue]] -> [a]
list f = catMaybes . map f

-- converter

account :: [SqlValue] -> Maybe Account
account [SqlInteger i] = Just $ Account i
account _              = Nothing

-- queries

selectAccounts :: Query [Account]
selectAccounts = Query
  "SELECT id FROM accounts"
  []
  (list account)

selectAccountByLogin :: String -> Query (Maybe Account)
selectAccountByLogin login = Query
  "SELECT id FROM accounts WHERE login = ?"
  [SqlString login]
  (one account)

-- run and transform query results
