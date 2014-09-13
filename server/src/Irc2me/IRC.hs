{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Irc2me.IRC where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Time
import Data.List
import Data.Text.Lens
import Irc2me.ProtoBuf.Messages hiding (_networkId)

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe

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

import Irc2me.IRC.Broadcast

type IrcConnections = Map AccountID (Map NetworkID IrcBroadcast)

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

          log' $ "Connecting to " ++ show (server ^. serverHost)
          ident <- require $ runQuery $ selectNetworkIdentity accid netid
          mbc'  <- liftIO $ startBroadcasting ident server
          case mbc' of
            Just bc -> do

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

------------------------------------------------------------------------------
-- Testing

test :: IO ()
test = (print =<<) $ runExceptT $ do
  connections <- reconnectAll Map.empty

  forM_ (Map.toList connections) $ \(acc, networks) -> do

    let log' s = putStrLn $ "[" ++ show (_accountId acc) ++ "] " ++ s

    forM_ (Map.toList networks) $ \(_netid, bc) -> do
      liftIO $ forkIO $ subscribe bc $ log' . testFormat

  liftIO $ do
    void getLine `finally` forM_ (Map.toList connections) (\(_acc, networks) -> do
      forM_ (Map.toList networks) $ \(_, bc) -> do
        stopBroadcasting bc Nothing
      )

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
