{-# LANGUAGE RankNTypes #-}

module Database.Query where

import Control.Applicative
import Data.Maybe
import Database.HDBC

import Database.Config

data Query a = Query
  { queryStr  :: String
  , qParam    :: [SqlValue]
  , qConvert  :: [[SqlValue]] -> a
  }

data Update = Update
  { updateStr :: String
  , uParam    :: [SqlValue]
  }

runQuery :: Query a -> IO a
runQuery (Query s v conv) = runSQL $ \c ->
  conv <$> quickQuery' c s v

runUpdate :: Update -> IO Integer
runUpdate (Update s v) = runSQL $ \c -> do
  i <- run c s v
  commit c
  return i

runUpdate_ :: Update -> IO ()
runUpdate_ u = () <$ runUpdate u

-- conversion helpers

type Converter a = [SqlValue] -> Maybe a

convertList :: Converter a -> [[SqlValue]] -> [a]
convertList f = catMaybes . map f

convertOne :: Converter a -> [[SqlValue]] -> Maybe a
convertOne f = listToMaybe . convertList f
