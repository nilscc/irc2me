{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import Control.Concurrent
import Control.Monad

import qualified Data.ByteString.Char8 as B8
import           Data.Function
import           Data.Time

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

  Just (con,msgs) <- connect server OptionalSTARTTLS user

  -- output all debugging messages
  void $ forkIO $ fix $ \loop -> do
    ms <- getDebugOutput con
    case ms of
      Nothing -> return ()
      Just s  -> do
        putStrLn s
        loop

  nick <- getCurrentNick con
  mapM_ (printMessage nick) msgs

  mapM_ (send con . uncurry joinMsg) channels

  -- output incoming messages
  fix $ \loop -> do
    is_open <- isOpenConnection con
    if is_open then do
      mmsg <- getIncoming con
      case mmsg of
        Nothing -> loop
        Just msg -> do
          cur_nick <- getCurrentNick con
          printMessage cur_nick msg
          loop
     else
      putStrLn "Disconnected."

printMessage :: Nickname -> (UTCTime, Message) -> IO ()
printMessage cur_nick msgWithTime = do

  case msgWithTime of
    (_time, MOTDMsg motd) -> do
      putStrLn $ "> MOTD : " ++ B8.unpack motd

    (_time, TopicMsg _chan _topic) -> do
      putStrLn $ "> Topic changed." -- TODO

    (_time, NamreplyMsg chan namesWithFlags) -> do
      let showFlag Nothing = ""
          showFlag (Just Operator) = "@"
          showFlag (Just Voice) = "+"
      putStrLn $ "> " ++ B8.unpack chan ++ " names: "
                 ++ unwords [ showFlag flag ++ B8.unpack name
                            | (name,flag) <- namesWithFlags ]


    (_time, PrivMsg (Left from) to msg) -> do
      putStrLn $ "> " ++ B8.unpack to ++ " : "
                 ++ "<" ++ B8.unpack (userNick from) ++ "> "
                 ++ B8.unpack msg
    (_time, PrivMsg (Right _) to msg) -> do
      putStrLn $ "> " ++ B8.unpack to ++ " : " ++ B8.unpack msg

    (_time, NoticeMsg (Left from) to msg) -> do
      putStrLn $ "> NOTICE " ++ B8.unpack to ++ " : "
                 ++ "<" ++ B8.unpack (userNick from) ++ "> "
                 ++ B8.unpack msg
    (_time, NoticeMsg (Right _) to msg) -> do
      putStrLn $ "> NOTICE " ++ B8.unpack to ++ " : "
                 ++ B8.unpack msg

    (_time, JoinMsg chan Nothing) -> do
      putStrLn $ "> Joined " ++ show chan
    (_time, JoinMsg chan (Just who)) -> do
      putStrLn $ "> " ++ B8.unpack (userNick who)
                 ++ " joined " ++ show chan

    (_time, PartMsg chan who) -> do
      putStrLn $ "> " ++ show who ++ " left " ++ show chan ++ "."

    (_time, KickMsg chan who comment) -> do
      putStrLn $ "> Kick! " ++ show who ++ " from " ++ show chan ++ "."
                 ++ " Reason: " ++ show comment

    (_time, QuitMsg (Just who) comment) -> do
      putStrLn $ "> " ++ show (userNick who) ++ " disconnected."
                 ++ maybe "" ((" Reason: " ++) . show) comment
    (_time, QuitMsg Nothing _comment) -> do
      putStrLn $ "> Disconnected."

    (_time, NickMsg (Just old) new) -> do
      putStrLn $ "> " ++ B8.unpack (userNick old) ++ " is now called "
                 ++ B8.unpack new
    (_time, NickMsg Nothing new) -> do
      putStrLn $ "> You are now called " ++ B8.unpack new

    (_time, ErrorMsg code)
      | code == read err_NICKNAMEINUSE ||
        code == read err_NICKCOLLISION -> do
        putStrLn $ "! Nickname already taken."
      | otherwise -> do
        putStrLn $ "! EROR : " ++ show code

    (_time, OtherMsg from cmd params content) -> do

      let who | Just (Left (userNick -> nick)) <- from =
                "<" ++ B8.unpack nick ++ "> "
              | otherwise = ""

          pars | (h:t) <- params, cur_nick == h = t
               | otherwise = params

          cont | null pars = B8.unpack content
               | otherwise = B8.unpack (B8.unwords pars) ++ " "
                          ++ B8.unpack content

      putStrLn $ "? [" ++ B8.unpack cmd ++ "] " ++ who ++ cont

