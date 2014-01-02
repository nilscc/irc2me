{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad

import qualified Data.ByteString.Char8 as B8

import Network

--import System.IO

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

channels :: [(Channel, Maybe Key)]
channels = [ ("##test", Nothing) ]

main :: IO ()
main = do

  Just con <- connect user server channels

  -- output all debugging messages
  let loop = do ms <- getDebugOutput con
                case ms of
                  Nothing -> return ()
                  Just s  -> do
                    putStrLn s
                    loop
   in void $ forkIO loop

  -- output incoming messages
  let loop = do
        mmsg <- getIncomingMessage con
        case mmsg of
          Nothing -> return ()
          Just (_time, PrivMsg from to msg) -> do
            putStrLn $ B8.unpack to ++ ": "
                       ++ "<" ++ show from ++ "> "
                       ++ B8.unpack msg
            loop
   in void $ forkIO loop

  -- loop over incoming messages
  let loop con' = do
        open <- isOpenConnection con'
        when open $ do
          con'' <- handleIncoming con'

          loop con''
   in loop con
