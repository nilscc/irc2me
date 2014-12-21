module Main where

import Network
import Irc2me.Frontend.Connection

main :: IO ()
main = withSocketsDo $
  runServer defaultServerConfig
