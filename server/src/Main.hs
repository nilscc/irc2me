module Main where

import Irc2me.Frontend.Connection

main :: IO ()
main = runServer defaultServerConfig
