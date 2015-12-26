{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Irc2me.Backends.IRC.Events where

import Control.Concurrent
import Control.Monad.Trans

import Data.Function
import qualified Data.Foldable as F

-- containers
import qualified Data.Map as Map

-- lens
import Control.Lens

-- local

import Control.Concurrent.Broadcast
import Control.Concurrent.Event

import Irc2me.Events.Types
import Irc2me.Backends.IRC.Helper
import Irc2me.Backends.IRC.Types
import Irc2me.Database.Tables.Accounts

--------------------------------------------------------------------------------
-- Managing IRC connections

manageIrcConnections :: MonadIO m => IrcConnections -> EventT 'RW Event m ()
manageIrcConnections = fix $ \loop irc -> do

  Event <- getEvent

{-
  AccountEvent aid ev <- getEvent
  case ev of

    -- new client connected
    Ev_ClientConnected sendMsg -> do

      logM $ "Subscribe client (Account #" ++ show (aid ^. accountId) ++ ") to IRC networks"

      -- lookup all broadcasts for account
      F.forM_ (Map.findWithDefault Map.empty aid irc) $ \bc -> do

        -- subscribe client to broadcast
        liftIO $ forkIO $ subscribe bc sendMsg

    SendMessage nid msg

        -- look up account
      | Just nw <- Map.lookup aid irc
        -- look up network broadcast
      , Just bc <- Map.lookup nid nw -> do

        now <- liftIO getCurrentTime
        logM $ "Sending: " ++ testFormat (now,msg)
        BC.sendIrcMessage bc msg

    _ -> return ()

  -}
  loop irc
 where
  logM msg = liftIO $ putStrLn $ "[IRC] " ++ msg
