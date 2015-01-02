{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc2me.Backends.IRC
  ( runIrcBackend, runIrcBackend'
  ) where

import Control.Concurrent

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Int

import System.IO

-- time
import Data.Time.Clock.POSIX

-- irc-bytestring
import Network.IRC.ByteString.Parser as IRC

-- containers
import qualified Data.Map as Map

-- hdbc
import Database.HDBC

-- lens
import Control.Lens hiding (Identity)
import Data.Text.Lens

-- local
import Control.Concurrent.Event
import Irc2me.Events

import Irc2me.Frontend.Messages        as M hiding (_networkId)
-- import Irc2me.Frontend.Messages.Helper as M

import Irc2me.Database.Query            as DB
import Irc2me.Database.Tables.Accounts  as DB
import Irc2me.Database.Tables.Networks  as DB

import Irc2me.Backends.IRC.Helper
import Irc2me.Backends.IRC.NetworkState
import Irc2me.Backends.IRC.Broadcast as BC
import Irc2me.Backends.IRC.Events

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

          -- query network identity from DB
          ident <- require $ runQuery $ selectNetworkIdentity accid netid

          -- init network state
          ns <- newNetworkState accid netid ident
          let converter = evalIRCMsg ns

          -- start broadcasting
          mbc'  <- liftIO $ startIrcBroadcast server ident converter
          case mbc' of
            Just (con',bc) -> do

              log' $ "Connected to "
                ++ (server ^. serverHost . _Just . _Text)
                ++ ":"
                ++ show (server ^. serverPort . non 0)
                ++ " ("
                ++ (if server ^. serverUseTLS . non False then "using TLS" else "plaintext")
                ++ ")"

              -- store new broadcast
              at accid . non' _Empty . at netid ?= NetworkConnection bc con' ns

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
-- IRC message evaluation & network state

evalIRCMsg :: NetworkState -> (UTCTime, IRCMsg) -> IO (Maybe ServerMessage)
evalIRCMsg ns (t,msg) = do

  updateNetworkState ns msg

  -- get network identity
  ident <- getNetworkIdentitiy ns

  -- get network ID
  let nid = ns ^. nsNetworkID

  -- convert time + irc message to protobuf 'ChatMessage'
  let epoch :: Int64
      epoch = floor $ utcTimeToPOSIXSeconds t * 1000
      cm    = msg ^. chatMessage & messageTimestamp .~ Just epoch

  return $ Just $ buildServerMessage nid ident cm

updateNetworkState :: NetworkState -> IRCMsg -> IO ()
updateNetworkState ns msg = do
  
  --case 
  return ()

buildServerMessage :: NetworkID -> Identity -> ChatMessage -> ServerMessage
buildServerMessage nid ident cm = serverMessage $ network &~ do

  case messageChannelName of

    -- channel message
    Just chan -> do
      networkChannels .= [
        emptyChannel &~ do
          channelName .= Just chan
          channelMessages .= [ cm ]
        ]

    -- private message
    Nothing -> networkMessages .= [ cm ]

 where

  messageChannelName
    | Just _ty  <- cm ^. messageType        -- known type
    , [to']     <- cm ^. messageParams      -- one recipient
    , Just nick <- ident ^. identityNick
    , nick /= to'                           -- recipient does not match current
                                            -- nickname
    = Just to'

    | otherwise
    = Nothing

  -- ServerMessage default 'template'
  serverMessage netw = emptyServerMessage &
    serverNetworks .~ [ netw ]

  -- network template
  network = emptyNetwork &
    M.networkId .~ Just (nid ^. DB.networkId & fromIntegral)
