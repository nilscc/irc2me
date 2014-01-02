import Control.Concurrent
import Control.Monad

import Network

import System.IO

-- our imports:
import IRC
import Types

user :: User
user = User { usr_nick     = "McManiaC"
            , usr_nick_alt = ["irc2mob"]
            , usr_name     = "irc2mob"
            , usr_realname = "irc2mob"
            }

server :: Server
server = Server { srv_host = "irc.xinutec.org"
                , srv_port = PortNumber 6667
                }

main :: IO ()
main = do

  Just con <- connect user server

  closeConnection con Nothing

  let loop = do ms <- getDebugOutput con
                case ms of
                  Nothing -> return ()
                  Just s  -> do
                    putStrLn s
                    loop
   in loop
