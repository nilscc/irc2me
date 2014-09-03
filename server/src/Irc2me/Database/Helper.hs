module Irc2me.Database.Helper where

--
-- Simple helpers
--

as :: String -> String -> String
a `as` b = a ++ " AS " ++ b

qualified :: String -> [String] -> [String]
qualified i = map ((i ++ ".") ++)
