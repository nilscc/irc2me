{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Event
import qualified Data.Foldable as F
import Data.Time
import Data.List
import Data.Text.Lens
import Irc2me.Frontend.Messages hiding (_networkId)

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe

import System.IO

-- containers
import           Data.Map (Map)
import qualified Data.Map as Map

-- hdbc
import Database.HDBC

-- lens
import Control.Lens

-- local
import Irc2me.Database.Query
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks

import Irc2me.Events
import Irc2me.Backends.IRC.Broadcast as BC

type IrcConnections = Map AccountID (Map NetworkID IrcBroadcast)

runIrcBackend :: MonadIO m => EventT mode AccountEvent  m Bool
runIrcBackend = do
  eq <- getEventQueue
  runIrcBackend' eq

runIrcBackend' :: MonadIO m => EventQueue WO AccountEvent -> m Bool
runIrcBackend' eq = do
  mcs <- runExceptT $ reconnectAll Map.empty
  case mcs of
    Right cs -> do
      _  <- liftIO $ forkIO $ runEventTRW eq $
        manageIrcConnections cs
      return True
    Left err -> do
      liftIO $ hPutStrLn stderr $
        "[IRC] Failed to start backend, reason: " ++ show err
      return False

--------------------------------------------------------------------------------
-- Starting up

reconnectAll
  :: (MonadIO m, MonadError SqlError m)
  => IrcConnections
  -> m IrcConnections
reconnectAll con = withCon con $ do

  accs <- runQuery selectAccounts
  for accs $ \accid -> do

    let log' s = liftIO . putStrLn $ "[" ++ show (_accountId accid) ++ "] " ++ s

    servers <- runQuery $ selectServersToReconnect accid
    for servers $ \(netid, server) -> do

      -- lookup network to see if we're already connected
      mbc <- preuse $ at accid . _Just . at netid
      case mbc of

        -- network already connected
        Just _ -> return ()

        -- reconnect network
        Nothing -> do

          ident <- require $ runQuery $ selectNetworkIdentity accid netid
          mbc'  <- liftIO $ startBroadcasting ident (netid,server)
          case mbc' of
            Just bc -> do

              log' $ "Connected to "
                ++ (server ^. serverHost . _Just . _Text)
                ++ ":"
                ++ show (server ^. serverPort . non 0)
                ++ " ("
                ++ (if server ^. serverUseTLS . non False then "using TLS" else "plaintext")
                ++ ")"

              _ <- liftIO $ forkIO $ subscribe bc $ \_nid tmsg ->
                putStrLn $ "[IRC] " ++ testFormat tmsg

              -- store new broadcast
              at accid . non' _Empty . at netid ?= bc

            Nothing -> do
              log' $ "Failed to connect to Network " ++ show (_networkId netid)

 where
  for :: Monad m => [a] -> (a -> MaybeT m ()) -> m ()
  for   l m = do
    _ <- runMaybeT $ forM_ l (\a -> m a `mplus` return ())
    return ()

  require m = maybe mzero return =<< m
  withCon c = execStateT `flip` c

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

------------------------------------------------------------------------------
-- Testing

testFormat :: (UTCTime, IrcMessage) -> String
testFormat (t, msg) =

  let time = show t -- formatTime defaultTimeLocale "%T" t

      cmd = (    msg ^? ircMessageType    . _Just . re _Show
             <|> msg ^? ircMessageTypeRaw . _Just . _Text
            ) ^. non "?"

      who = (    msg ^? ircFromUser   . _Just . userNick . _Text
             <|> msg ^? ircFromServer . _Just . _Text
            ) ^. non "-"

      par = msg ^. ircTo ^.. traversed . _Text & intercalate ", "

      cnt = (msg ^? ircContent . _Just . _Text) ^. non ""

  in "["  ++ time ++ "]"
  ++ " "  ++ cmd
  ++ " <" ++ who ++ ">"
  ++ " [" ++ par ++ "]"
  ++ " "  ++ cnt
