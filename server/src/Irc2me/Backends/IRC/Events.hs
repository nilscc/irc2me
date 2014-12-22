{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC.Events where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import Data.Time
import Data.Time.Clock.POSIX

import qualified Data.Foldable as F

-- containers
import qualified Data.Map as Map

-- lens
import Control.Lens

-- local

import Control.Concurrent.Broadcast
import Control.Concurrent.Event

import Irc2me.Events
import Irc2me.Backends.IRC.Helper
import qualified Irc2me.Frontend.Messages as Messages
import Network.IRC.Connection
import Irc2me.Database.Tables.Accounts

--------------------------------------------------------------------------------
-- Managing IRC connections

manageIrcConnections :: MonadIO m => IrcConnections -> EventRW m ()
manageIrcConnections irc = forever $ do

  AccountEvent aid ev <- getEvent
  case ev of

    -- new client connected
    ClientConnected sendmsg -> do

      logM $ "Subscribe client (Account #" ++ show (aid ^. accountId) ++ ") to IRC networks"

      -- lookup all broadcasts for account
      F.forM_ (Map.findWithDefault Map.empty aid irc) $ \netcon -> do

        -- subscribe client to broadcast
        liftIO $ forkIO $ subscribe (netcon ^. networkBroadcast) sendmsg

    SendMessage nid msg

        -- look up account
      | Just nw <- Map.lookup aid irc
        -- look up network broadcast
      , Just nc <- Map.lookup nid nw -> liftIO $ do

        -- add timestamp to msg
        now <- getCurrentTime
        let epoch = floor $ utcTimeToPOSIXSeconds now * 1000
            tmsg  = msg & Messages.messageTimestamp .~ Just epoch

        -- convert to irc message
        let ircmsg = tmsg ^. from Messages.chatMessage

        logM $ "Sending: " ++ testFormat (now, ircmsg)

        sendIrc (nc ^. networkConnection) ircmsg

        -- build server message for "rebroadcast"
        rebroadcast (nc ^. networkBroadcast) nid tmsg

      | otherwise -> logM $ "Network not found."

 where
  logM msg = liftIO $ putStrLn $ "[IRC] " ++ msg
