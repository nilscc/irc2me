{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module IRC
  ( runIRC
  , module IRC.Types
  ) where

import Control.Concurrent

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import           Data.Function
import           Data.Time

import Network.IRC.ByteString.Parser

-- our imports:
import IRC.Connection
import IRC.Connection.State
import IRC.Codes
import IRC.Types

rejoinChannels :: Connection -> IO ()
rejoinChannels con = do
  chans <- getChannels con
  mapM_ (send con . uncurry joinMsg) $ M.toList chans

runIRC :: User -> Server -> [(Channel, Maybe Key)] -> IO ()
runIRC usr srv chans = do

  mcon <- connect srv usr
  case mcon of
    Nothing -> connectionFailed srv
    Just (con,msgs) -> do

      _ <- startDebuggingOutput con

      -- join new channels
      setChannels con chans
      rejoinChannels con

      nick <- getCurrentNick con
      mapM_ (printMessage nick) msgs

      runWithConnection con

reuseConnection :: Connection -> IO ()
reuseConnection old_con = do
  mcon <- reconnect old_con
  case mcon of
    Nothing -> connectionFailed (con_server old_con)
    Just (con,msgs) -> do

      nick <- getCurrentNick con
      mapM_ (printMessage nick) msgs

      runWithConnection con

runWithConnection :: Connection -> IO ()
runWithConnection con = do

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
     else if srv_reconnect (con_server con) then
      reuseConnection con
     else
      putStrLn "Disconnected."

--------------------------------------------------------------------------------
-- Output

startDebuggingOutput :: Connection -> IO ThreadId
startDebuggingOutput con = do
  -- output all debugging messages
  forkIO $ fix $ \loop -> do
    ms <- getDebugOutput con
    case ms of
      Nothing -> return ()
      Just s  -> do
        putStrLn s
        loop

connectionFailed :: Server -> IO ()
connectionFailed srv = do
  -- TODO: error message!
  putStrLn $ "Could not connect to server " ++ srv_host srv

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

