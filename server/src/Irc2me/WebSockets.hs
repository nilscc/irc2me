{-# LANGUAGE DataKinds #-}

module Irc2me.WebSockets
  ( runWebSockets
  ) where

--import Control.Concurrent.Event
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

  [ dir "account" $ msum

    [ dir "create" $ routeAccept $ \con -> do
        bs <- receiveData con
        case decode bs of
          Just (Account login (Just pw)) -> do
            encpw <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 pw)
            mid <- runExceptT $ DB.runUpdate $ DB.createAccount login encpw
            case mid of
              Right (Just _i) -> sendTextData con $ encode StatusOK
              _               -> sendTextData con $ encode (StatusFailed Nothing)
          _ -> sendTextData con $ encode (StatusFailed Nothing)

    , dir "auth" $ routeAccept $ \con -> do
      bs <- receiveData con
      case decode bs of
        Just (Account login (Just pw)) -> do
          mepw <- runExceptT $ DB.runQuery $ DB.selectAccountPassword login
          case mepw of
            Right (Just epw) -> do
              if verifyPass' (Pass $ encodeUtf8 pw) epw then do
                sendTextData con $ encode StatusOK
               else
                sendTextData con $ encode (StatusFailed Nothing)
            _ ->
              sendTextData con $ encode (StatusFailed Nothing)
        _ -> sendTextData con $ encode (StatusFailed Nothing)
    ]
  ]
