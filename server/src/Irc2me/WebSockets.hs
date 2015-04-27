{-# LANGUAGE DataKinds #-}

module Irc2me.WebSockets
  ( runWebSockets
  ) where

--import Control.Concurrent.Event
import Control.Monad
import Control.Monad.Trans

import Network.WebSockets
import Network.WebSockets.Routing
import Network.WebSockets.TLS

-- import Irc2me.Events
import Irc2me.Types

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

  [ dir "echo" $ msum

    [ dir "twice" $ routeAccept $ \con -> forever $ do
        -- echo twice
        s <- receive con
        send con s
        send con s

    , routeAccept $ \con -> forever $ do
        -- echo once
        s <- receive con
        send con s
    ]

  ]
