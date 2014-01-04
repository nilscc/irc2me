{-# LANGUAGE ScopedTypeVariables #-}

module IRC.Debug where

import Control.Concurrent.STM
import Control.Exception

import IRC.Types

--------------------------------------------------------------------------------
-- Debugging

logC :: Connection -> String -> IO ()
logC con s = do
  atomically $ writeTChan (con_debug_output con) s

logE, logW, logI :: Connection -> String -> String -> IO ()

logE con where_ what =
  logC con $ "Error (" ++ where_ ++ "): " ++ what

logW con where_ what =
  logC con $ "Warning (" ++ where_ ++ "): " ++ what

logI con where_ what =
  logC con $ "Info (" ++ where_ ++ "): " ++ what

getDebugOutput :: Connection -> IO (Maybe String)
getDebugOutput con = handleExceptions $ do
  s <- atomically $ readTChan (con_debug_output con)
  return $ Just s
 where
  handleExceptions = handle (\(_ :: BlockedIndefinitelyOnSTM) -> return Nothing)
