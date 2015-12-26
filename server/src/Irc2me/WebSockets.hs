{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.WebSockets
  ( runWebSockets
  ) where

--import Control.Concurrent.Event
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Crypto.Scrypt
import Data.Aeson
import Data.Text.Encoding

import Network.WebSockets
import Network.WebSockets.Routing
import Network.WebSockets.TLS

-- import Irc2me.Events
import Irc2me.Types
import qualified Irc2me.Database.Query as DB
import qualified Irc2me.Database.Tables.Accounts as DB

-- | Run a websockets server using TLS
runWebSockets
  :: MonadIO m
  => ServerConfig
  -- -> EventQueue WO Event
  -> m ()
runWebSockets (ServerConfig host port cert key mca) = liftIO $ do
  runSimpleTLSServer host port cert key mca $
    routeWebSockets mainRoute

-- | The main WebSockets route
mainRoute :: WebSocketsRoute ()
mainRoute = msum

  ---------------------------------------------------------------------------
  -- ACCOUNT ROUTES

  [ dir "account" $ msum

    [ dir "create" $ accept (failMsg "Failed to create account.") $ \con -> do
        Account login (Just pw) <- receiveDecoded con
        -- encrypt the password
        encpw     <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 pw)
        -- store the encrypted pw in the DB. returns the ID if successful
        Just _id  <- runUpdate $ DB.createAccount login encpw
        send' con StatusOK

    , dir "auth" $ accept (failMsg "Invalid login or password.") $ \con -> do
        Account login (Just pw) <- receiveDecoded con
        -- lookup the accounts encrypted password
        Just epw <- runQuery $ DB.selectAccountPassword login
        -- make sure the passwords match
        guard $ verifyPass' (Pass $ encodeUtf8 pw) epw
        send' con StatusOK
    ]

  ---------------------------------------------------------------------------
  -- IRC ROUTES

  , dir "irc" $ msum

    [
    ]

  ]
 where

  -- Directly send JSON messages
  send' con a = sendTextData con $ encode a

  -- Accept route and send fail message if it fails
  accept msgOnFail go = routeAccept $ \con -> go con <|> send' con msgOnFail
  failMsg msg = StatusFailed $ Just msg

  -- Receive JSON encoded messages (fails if parse not possible)
  receiveDecoded con = do
    Just d <- decode <$> receiveData con
    return d

  -- Wrapper for cleaner DB queries/updates
  runDB db = do
    Right e <- runExceptT db
    return e
  runUpdate u = runDB $ DB.runUpdate u
  runQuery  q = runDB $ DB.runQuery q
