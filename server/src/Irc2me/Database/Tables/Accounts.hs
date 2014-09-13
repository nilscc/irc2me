{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Irc2me.Database.Tables.Accounts where

import Data.ByteString (ByteString)
import Data.List

-- lens
import Control.Lens
import Data.Text.Lens

-- scrypt
import Crypto.Scrypt

-- hdbc
import Database.HDBC

-- local
import Irc2me.Database.Query
import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages.IrcIdentity

-- ID type

type ID = Integer

toID :: Converter ID
toID [SqlInteger i] = Just i
toID _              = Nothing

--
-- Accounts
--

newtype AccountID = AccountID { _accountId :: Integer }
  deriving (Eq, Show, Ord, Num)

-- converters

accountSELECT :: String
accountSELECT = "SELECT id FROM accounts"

toAccountID :: Converter AccountID
toAccountID = \case
  [SqlInteger i] -> Just $ AccountID i
  _              -> Nothing

-- queries

selectAccounts :: Query [AccountID]
selectAccounts = Query
  accountSELECT
  []
  (convertList toAccountID)

selectAccountByLogin :: String -> Query (Maybe AccountID)
selectAccountByLogin login = Query
  (accountSELECT ++ " WHERE login = ?")
  [toSql login]
  (convertOne toAccountID)

checkPassword :: AccountID -> ByteString -> Query Bool
checkPassword (AccountID acc) pw = Query
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
  -> Update (Maybe AccountID)
addAccount login encrypted = UpdateReturning
  "INSERT INTO accounts (login, password) VALUES (?, ?) \
  \  RETURNING id"
  [toSql login, byteaPack (getEncryptedPass encrypted)]
  (convertOne toAccountID)

--------------------------------------------------------------------------------
-- "account_identities" table

-- converters

identityFields :: [String]
identityFields = ["id", "username", "realname", "nick", "nick_alt"]


identityTable :: String
identityTable = "account_identities"

identitySELECT :: String
identitySELECT = "SELECT " ++ intercalate ", " identityFields ++ " FROM " ++ identityTable

toIrcIdentity :: Converter IrcIdentity
toIrcIdentity s = case s of
  [SqlInteger i, SqlByteString u, SqlByteString r, SqlByteString n, na] -> Just $
    emptyIrcIdentity &~ do
      identityId       .= Just (fromIntegral i)
      identityName     .= Just (u ^. encoded)
      identityRealname .= Just (r ^. encoded)
      identityNick     .= Just (n ^. encoded)
      identityNickAlt  .= maybe [] (map (^. re _Text)) (arrayUnpack na)
  _ -> Nothing

-- queries

selectIdentities :: AccountID -> Query [IrcIdentity]
selectIdentities (AccountID a) = Query
  (identitySELECT ++ " WHERE account = ? ORDER BY nick")
  [toSql a]
  (convertList toIrcIdentity)

-- updates

addIrcIdentity :: AccountID -> IrcIdentity -> Update (Maybe ID)
addIrcIdentity (AccountID a) ident = UpdateReturning
  "INSERT INTO account_identities (account, username, realname, nick, nick_alt) \
  \     VALUES                    (?      , ?       , ?       , ?   , ?       ) \
  \  RETURNING id"
  [ toSql     $ a
  , toSql     $ ident ^. identityName
  , toSql     $ ident ^. identityRealname
  , toSql     $ ident ^. identityNick
  , arrayPack $ ident ^. identityNickAlt ^.. folded . _Text
  ]
  (convertOne toID)

deleteIrcIdentity :: AccountID -> ID -> Update Bool
deleteIrcIdentity (AccountID a) i = Update
  "DELETE FROM account_identities WHERE account = ? AND id = ?"
  [ toSql a
  , toSql i
  ]
  (== 1)

setIrcIdentity :: AccountID -> IrcIdentity -> Update Bool
setIrcIdentity (AccountID a) ident = Update
  "UPDATE account_identities \
  \   SET username = ?, realname = ?, nick = ?, nick_alt = ?  \
  \ WHERE account = ? AND id = ?"
  [ toSql     $ ident ^. identityName
  , toSql     $ ident ^. identityRealname
  , toSql     $ ident ^. identityNick
  , arrayPack $ ident ^. identityNickAlt ^.. folded . _Text
  , toSql     $ a
  , toSql     $ ident ^. identityId
  ]
  (== 1)
