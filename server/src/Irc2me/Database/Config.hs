module Irc2me.Database.Config where

import Database.HDBC
import Database.HDBC.PostgreSQL

conStr :: String
conStr = "host=localhost user=nils dbname=irc2me_test"

runSQL :: (Connection -> IO a) -> IO (Either SqlError a)
runSQL w = catchSql (withPostgreSQL conStr (fmap Right . w))
                    (return . Left)
