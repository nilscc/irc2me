module Irc2me.Backends.IRC.Events where

import Control.Concurrent
import Control.Monad.Trans

import Data.Function
import Data.Time
import qualified Data.Foldable as F

-- containers
import qualified Data.Map as Map

-- lens
import Control.Lens

-- local
import Control.Concurrent.Event
import Irc2me.Events

import Irc2me.Backends.IRC.Broadcast as BC
import Irc2me.Backends.IRC.Helper

import Irc2me.Database.Tables.Accounts

--------------------------------------------------------------------------------
-- Managing IRC connections

manageIrcConnections :: MonadIO m => IrcConnections -> EventRW m ()
manageIrcConnections = fix $ \loop irc -> do
  AccountEvent aid ev <- getEvent
  case ev of

    ClientConnected (IrcHandler h) -> do
      logM $ "Subscribe client (Account #" ++ show (aid ^. accountId) ++ ") to IRC networks"
      F.forM_ (Map.findWithDefault Map.empty aid irc) $ \bc ->
        liftIO $ forkIO $ subscribe bc h

    SendIrcMessage nid msg

        -- look up account
      | Just nw <- Map.lookup aid irc
        -- look up network broadcast
      , Just bc <- Map.lookup nid nw -> do

        now <- liftIO getCurrentTime
        logM $ "Sending: " ++ testFormat (now,msg)
        BC.sendIrcMessage bc msg

    _ -> return ()

  loop irc
 where
  logM msg = liftIO $ putStrLn $ "[IRC] " ++ msg
