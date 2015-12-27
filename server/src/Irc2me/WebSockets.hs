{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Irc2me.WebSockets
  ( runWebSockets
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Crypto.Scrypt
import Data.Aeson
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy (ByteString)

import Network.WebSockets as WS
import Network.WebSockets.Routing
import Network.WebSockets.TLS

import Control.Concurrent.Event
--import Irc2me.Events
import Irc2me.Events.Types
import Irc2me.Types
import qualified Irc2me.Database.Query as DB
import qualified Irc2me.Database.Tables.Accounts as DB

-- | Run a websockets server using TLS
runWebSockets
  :: MonadIO m
  => ServerConfig
  -> EventQueue 'WO AccountEvent
  -> m ()
runWebSockets (ServerConfig host port cert key mca) eq = liftIO $ do
  runSimpleTLSServer host port cert key mca $
    routeWebSockets (mainRoute eq)

-- | The main WebSockets route
mainRoute
  :: EventQueue 'WO AccountEvent
  -> WebSocketsRoute ()
mainRoute eq = msum

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
    ]

  ---------------------------------------------------------------------------
  -- MAIN ROUTE

  , dir "main" $ accept (failMsg "Invalid login or password.") $ \con -> do

      --
      -- Authetication
      --

      Account login (Just pw) <- receiveDecoded con
      -- lookup the accounts encrypted password
      Just (aid, epw) <- runQuery $ DB.selectAccountPassword login
      -- make sure the passwords match
      guard $ verifyPass' (Pass $ encodeUtf8 pw) epw
      send' con StatusOK

      -- raise event to notify any backends
      raiseEvent' eq $ AccountEvent aid $ ClientConnected (liftIO . send' con)

      --
      -- Message processing
      --

      forever $ do

        -- incoming data
        (bs :: ByteString) <- receiveData con

        -- data parsing
        let withData
              :: (FromJSON a, MonadPlus m, MonadIO m, ToJSON b)
              => (a -> m b) -> m ()
            withData go = do
              Just (WebSocketMessage i dat) <- return $ decode bs
              response <- go dat
              liftIO $ sendTextData con $ encode $ WebSocketMessage i response

        -- MonadPlus fail catcher
        let onFail msg go = go
                         <|> withData (\(_ :: Value) -> return $ failMsg msg)
                         <|> liftIO (putStrLn $ "Impossible parse: " ++ show bs)

        --
        -- Handle requests
        --

        onFail "Unexpected/invalid request." $ msum
          [ withData $ \(s::T.Text) -> do
              return s
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
