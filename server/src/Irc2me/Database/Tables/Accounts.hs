{-# LANGUAGE LambdaCase #-}

module Irc2me.Database.Tables.Accounts where

import Data.ByteString (ByteString)

-- lens
import Control.Lens hiding (Identity)
import Data.Text.Lens

-- scrypt
import Crypto.Scrypt

-- hdbc
import Database.HDBC

-- local
import Irc2me.Database.Query
import Irc2me.ProtoBuf.Helper
import Irc2me.ProtoBuf.Messages.Identity

-- ID type

type ID = Integer

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
    emptyIdentity &~ do
      identityId       .= Just (fromIntegral i)
      identityName     .= Just (u ^. encoded)
      identityRealname .= Just (r ^. encoded)
      identityNick     .= Just (n ^. encoded)
      identityNickAlt  .= maybe [] (map (^. re _Text)) (arrayUnpack na)
  _ -> Nothing

-- queries

selectIdentities :: Account -> Query [Identity]
selectIdentities (Account a) = Query
  (identitySELECT ++ " WHERE account = ? ORDER BY nick")
  [toSql a]
  (convertList toIdentity)

-- updates

addIdentity :: Account -> Identity -> Update (Maybe ID)
addIdentity (Account a) ident = UpdateReturning
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
  [ toSql     $ ident ^. identityName
  , toSql     $ ident ^. identityRealname
  , toSql     $ ident ^. identityNick
  , arrayPack $ ident ^. identityNickAlt ^.. folded . _Text
  , toSql     $ a
  , toSql     $ ident ^. identityId
  ]
  (== 1)
