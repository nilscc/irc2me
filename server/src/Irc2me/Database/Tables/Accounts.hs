module Irc2me.Database.Tables.Accounts where

import Database.HDBC

import Crypto.Scrypt
import Data.Text

import Irc2me.Database.Query

type ID = Integer
type AccountID = ID

toID :: Converter ID
toID [SqlInteger i] = Just i
toID _              = Nothing

selectAccounts :: Query [AccountID]
selectAccounts = Query
  "SELECT id FROM accounts" [] (convertList toID)

createAccount :: Text -> EncryptedPass -> Update (Maybe ID)
createAccount login pw = UpdateReturning
  "INSERT INTO accounts (login, password) VALUES (?, ?) \
  \RETURNING id"
  [ toSql login, byteaPack (getEncryptedPass pw) ]
  (convertOne toID)

selectAccountPassword
  :: Text  -- ^ Account login
  -> Query (Maybe (AccountID, EncryptedPass))
selectAccountPassword login = Query
  "SELECT id, password FROM accounts WHERE login = ?"
  [ toSql login ]
  (convertOne $ \s -> case s of
    [s_i,s_pw] -> do
      i  <- toInt     [s_i]
      pw <- fromBytea [s_pw]
      return (i, EncryptedPass pw)
    _ -> Nothing
  )
