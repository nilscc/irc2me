module Database.Config where

import Database.HDBC
import Database.HDBC.PostgreSQL

conStr :: String
conStr = "host=db.local user=nils dbname=irc2mobile-test"

runSQL :: (Connection -> IO a) -> IO (Either SqlError a)
runSQL w = catchSql (withPostgreSQL conStr (fmap Right . w))
                    (return . Left)
