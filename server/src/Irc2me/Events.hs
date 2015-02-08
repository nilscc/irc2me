module Irc2me.Events
  ( handleEvents
  , module Irc2me.Events.Types
  ) where

import Control.Monad
import Control.Monad.State
import System.IO

import qualified Data.Map as M

-- lens
import Control.Lens

-- local
import Control.Concurrent.Event

import Irc2me.Frontend.Connection.Types
import Irc2me.Backends.IRC.Helper

import Irc2me.Events.Types
import Irc2me.Events.ChatMessageEvent

handleEvents :: EventRW IO ()
handleEvents = evalStateT `flip` eventState $ forever $ do

  AccountEvent account e <- lift getEvent

  -- prism onto the current account state
  let accState      = elsAccounts . at account . non' _Empty
      preAccState l = preuse $ elsAccounts . at account . _Just . l
      useAccState l =    use $ elsAccounts . at account . _Just . l

  case e of

    -- New client connection
    ClientConnectedEvent cc -> do

      -- add client to account state
      accState . connectedClients ++= [cc]

    -- Incoming IRC/chat message
    ChatMessageEvent nid cm -> do

      -- look up identity of the current irc network & build server message
      ident <- preAccState $ connectedIrcNetworks . at nid . _Just . ircIdentity
      let response = buildChatMessageResponse nid ident cm

      -- send server message to all clients
      clients <- useAccState connectedClients
      forM_ clients $ \(ClientConnection _ send) -> do
        liftIO $ send response

    -- other
    _ -> liftIO $ hPutStrLn stderr $ "Unhandled event: " ++ show e

 where
  eventState :: EventLoopState
  eventState = EventLoopState M.empty

  infix 4 ++=
  l ++= a = l %= (++ a)
