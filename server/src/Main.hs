{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import Control.Concurrent
import Control.Monad

import qualified Data.ByteString.Char8 as B8

import Network
import Network.IRC.ByteString.Parser

--import System.IO

-- our imports:
import IRC
import IRC.Codes
import IRC.Types

user :: User
user = User { usr_nick     = "McManiaC"
            , usr_nick_alt = ["irc2mob"]
            , usr_name     = "irc2mob"
            , usr_realname = "irc2mob"
            }

server :: Server
server = Server { srv_host = "irc.xinutec.net"
                , srv_port = PortNumber 6667
                }

channels :: [(Channel, Maybe Key)]
channels = [ ("##test", Nothing) ]

main :: IO ()
main = do

  putStrLn "Connecting..."

  Just con <- connect server STARTTLS user

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

          Just (_time, MOTDMsg motd) -> do
            putStrLn $ "> MOTD : " ++ B8.unpack motd
            loop

          Just (_time, PrivMsg (Left from) to msg) -> do
            putStrLn $ "> " ++ B8.unpack to ++ " : "
                       ++ "<" ++ B8.unpack (userNick from) ++ "> "
                       ++ B8.unpack msg
            loop
          Just (_time, PrivMsg (Right _) to msg) -> do
            putStrLn $ "> " ++ B8.unpack to ++ " : " ++ B8.unpack msg
            loop

          Just (_time, NoticeMsg (Left from) to msg) -> do
            putStrLn $ "> NOTICE " ++ B8.unpack to ++ " : "
                       ++ "<" ++ B8.unpack (userNick from) ++ "> "
                       ++ B8.unpack msg
            loop
          Just (_time, NoticeMsg (Right _) to msg) -> do
            putStrLn $ "> NOTICE " ++ B8.unpack to ++ " : "
                       ++ B8.unpack msg
            loop

          Just (_time, JoinMsg chan Nothing) -> do
            putStrLn $ "> Joined " ++ show chan
            loop
          Just (_time, JoinMsg chan (Just who)) -> do
            putStrLn $ "> " ++ B8.unpack (userNick who)
                       ++ " joined " ++ show chan
            loop

          Just (_time, PartMsg chan who) -> do
            putStrLn $ "> " ++ show who ++ " left " ++ show chan ++ "."
            loop

          Just (_time, KickMsg chan who comment) -> do
            putStrLn $ "> Kick! " ++ show who ++ " from " ++ show chan ++ "."
                       ++ " Reason: " ++ show comment
            loop

          Just (_time, QuitMsg _chans (Just who) comment) -> do
            putStrLn $ "> " ++ show (userNick who) ++ " disconnected."
                       ++ maybe "" ((" Reason: " ++) . show) comment
            loop
          Just (_time, QuitMsg _chans Nothing _comment) -> do
            putStrLn $ "> Disconnected."
            loop

          Just (_time, NickMsg (Just old) new) -> do
            putStrLn $ "> " ++ B8.unpack (userNick old) ++ " is now called "
                       ++ B8.unpack new
            loop
          Just (_time, NickMsg Nothing new) -> do
            putStrLn $ "> You are now called " ++ B8.unpack new
            loop

          Just (_time, ErrorMsg code)
            | code == read err_NICKNAMEINUSE ||
              code == read err_NICKCOLLISION -> do
              putStrLn $ "! Nickname already taken."
              loop
            | otherwise -> do
              putStrLn $ "! EROR : " ++ show code
              loop

          Just (_time, OtherMsg from cmd params content) -> do

            cur_nick <- getCurrentNick con

            let who | Just (Left (userNick -> nick)) <- from =
                      "<" ++ B8.unpack nick ++ "> "
                    | otherwise = ""

                pars | (h:t) <- params, cur_nick == h = t
                     | otherwise = params

                cont | null pars = B8.unpack content
                     | otherwise = B8.unpack (B8.unwords pars) ++ " "
                                ++ B8.unpack content

            putStrLn $ "? [" ++ B8.unpack cmd ++ "] " ++ who ++ cont
            loop

   in void $ forkIO loop

  waitForInitialization con

  mapM_ (uncurry $ sendJoin con) channels

  -- loop over incoming messages
  let loop = do
        is_open <- isOpenConnection con
        if is_open then do
          handleIncoming con
          loop
         else
          putStrLn "Connection closed!"
   in loop
