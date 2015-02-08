module Irc2me.Events
  ( handleEvents
  , module Irc2me.Events.Types
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set

-- lens
import Control.Lens

-- local
import Control.Concurrent.Event

import Irc2me.Frontend.Connection.Types
--import Irc2me.Backends.IRC.Helper

import Irc2me.Events.Types
import Irc2me.Events.Helper
import Irc2me.Events.ChatMessageEvent
import Irc2me.Events.ClientMessageEvent

handleEvents :: EventRW IO ()
handleEvents = evalStateT `flip` eventState $ forever $ do

  AccountEvent account event <- lift getEvent

  -- prism onto the current account state
  let accState      = elsAccounts . at account . non' _Empty
      useAccState l = use $ elsAccounts . at account . _Just . l

  -- run a `ReaderT AccountState m` monad
  let useAccountState f = do
        as <- use $ elsAccounts . at account . non' _Empty
        runReaderT f as

  -- run a `StateT AccountState m` monad
  let withAccountState f = do
        as <- use $ elsAccounts . at account . non' _Empty
        (a, as') <- runStateT f (as)
        accState .= as'
        return a

  case event of

    {-
     - IRC events
     -
     -}

    NewIrcConnectionEvent nid con ident -> do

      let ircState = IrcState { _ircConnection = con
                              , _ircIdentity   = ident
                              , _ircChannels   = Set.empty
                              , _ircUsers      = Map.empty
                              }

      accState . connectedIrcNetworks %= Map.insert nid ircState

    -- Incoming IRC/chat message
    ChatMessageEvent nid cm -> do

      -- look up identity of the current irc network & build server message
      mresponse <- withAccountState $ do
        buildChatMessageResponse nid cm

      case mresponse of
        Nothing -> return ()
        Just response -> do
          -- send server message to all clients
          clients <- useAccState connectedClients
          forM_ clients $ \(ClientConnection _ send) -> do
            liftIO $ send response

    {-
     - Client events
     -
     -}

    -- New client connection
    ClientConnectedEvent cc -> do

      -- add client to account state
      accState . connectedClients ++= [cc]

    -- Incoming client message
    ClientMessageEvent cc cm -> do

      -- handle client message event
      success <- useAccountState $ clientMessageEvent account cc cm

      unless success $
        liftIO $ hPutStrLn stderr $ "Unhandled client message: " ++ show cm

 where
  eventState :: EventLoopState
  eventState = EventLoopState Map.empty

