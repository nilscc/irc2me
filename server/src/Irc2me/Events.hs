module Irc2me.Events
  ( handleEvents
  , module Irc2me.Events.Types
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

-- lens
import Control.Lens

-- local
import Control.Concurrent.Event

import Irc2me.Frontend.Connection.Types
--import Irc2me.Backends.IRC.Helper

import Irc2me.Events.Types
-- import Irc2me.Events.Helper
import Irc2me.Events.ChatMessageEvent
import Irc2me.Events.ClientMessageEvent

handleEvents :: EventRW IO ()
handleEvents = evalStateT `flip` eventState $ forever $ do

  event <- lift getEvent

  case event of

    {-
     - Client events
     -
     -}

    ClientEvent cid clientEvent -> do

      case clientEvent of

        -- New client connection
        ClientConnectedEvent cc -> do

          -- add client to account state
          elsClients %= Map.insert cid (ClientState cc Nothing)

        -- Client authenticated by frontend
        ClientAuthenticatedEvent aid -> do

          elsAccounts . at aid . non' _Empty . connectedClients %= Set.insert cid

        -- Client disconnected
        ClientDisconnectedEvent -> do

          elsClients %= Map.delete cid
          elsAccounts . each . connectedClients %= Set.delete cid

    {-
     - Account events
     -
     -}

    AccountEvent account accountEvent -> do

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

      case accountEvent of

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
        ChatMessageEvent nid mu cm -> do

          -- look up identity of the current irc network & build server message
          mresponse <- withAccountState $ do
            buildChatMessageResponse nid mu cm

          case mresponse of
            Nothing -> return ()
            Just response -> do

              -- get all connected clients of current account
              clientids <- useAccState connectedClients
              clients   <- Map.filterWithKey (\k _ -> Set.member k clientids) <$>
                             use elsClients

              -- send server message to all clients
              Foldable.forM_ clients $ \cs -> do
                let send = cs ^. clientConnection . ccSend
                liftIO $ send response

        {-
         - Client events
         -
         -}

        -- Incoming client message
        ClientMessageEvent cc cm -> do

          -- handle client message event
          success <- useAccountState $ clientMessageEvent account cc cm

          unless success $
            liftIO $ hPutStrLn stderr $ "Unhandled client message: " ++ show cm

 where
  eventState :: EventLoopState
  eventState = EventLoopState Map.empty Map.empty

