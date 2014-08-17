{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server.Connections

main :: IO ()
main = serverStart defaultServerConf

{-
user :: Identity
user = Identity { usr_nick     = "McManiaC"
                , usr_nick_alt = ["irc2mob"]
                , usr_name     = "irc2mob"
                , usr_realname = "irc2mob"
                }

freenode :: Server
freenode = Server { srv_host      = "irc.freenode.net"
                  , srv_port      = PortNumber 6697
                  , srv_tls       = TLS
                  , srv_reconnect = True
                  }

--------------------------------------------------------------------------------

main :: IO ()
main = do

  putStrLn "Connecting..."

  let server   = freenode
      channels = [ ("##test", Nothing) ]

  runIRC user server channels
-}
