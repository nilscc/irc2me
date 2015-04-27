module Irc2me.Database.Tables.Accounts where

import Database.HDBC

import Irc2me.Database.Query

type ID = Integer
type AccountID = ID

toID :: Converter ID
toID [SqlInteger i] = Just i
toID _              = Nothing

selectAccounts :: Query [AccountID]
selectAccounts = Query
  "SELECT id FROM accounts" [] (convertList toID)
