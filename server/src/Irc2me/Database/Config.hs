{-# LANGUAGE FlexibleContexts #-}

module Irc2me.Database.Config where

import Control.Monad.Except

import Database.HDBC
import Database.HDBC.PostgreSQL

conStr :: String
conStr = "host=localhost user=nils dbname=irc2me-test"

runSQL
  :: (MonadIO m, MonadError SqlError m)
  => (Connection -> IO a) -> m a
runSQL w = do
  r <- liftIO $ catchSql (withPostgreSQL conStr (fmap Right . w))
                         (return . Left)
  case r of
    Left  e -> throwError e
    Right a -> return a
