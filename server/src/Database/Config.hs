module Database.Config where

import Database.HDBC.PostgreSQL

conStr :: String
conStr = "host=db.local user=nils dbname=irc2mobile-test"

runSQL :: (Connection -> IO a) -> IO a
runSQL w = withPostgreSQL conStr w
