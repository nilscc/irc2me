{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Query where

import Control.Applicative
import Data.Maybe
import Data.List
import Data.Word
import Database.HDBC
import Numeric
import Text.Parsec hiding (many)
import Text.Parsec.String
import Text.Printf

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Database.Config

data Query a = Query
  { queryStr  :: String
  , qParam    :: [SqlValue]
  , qConvert  :: [[SqlValue]] -> a
  }

data Update a
  = Update
      { uStr      :: String
      , uParam    :: [SqlValue]
      , uConvert  :: Integer -> a
      }
  | UpdateReturning
      { urStr      :: String
      , urParam    :: [SqlValue]
      , urConvert  :: [[SqlValue]] -> a
      }

runQuery :: Query a -> IO (Either SqlError a)
runQuery (Query s v conv) = runSQL $ \c ->
  conv <$> quickQuery' c s v

runUpdate :: Update a -> IO (Either SqlError a)
runUpdate (Update s v conv) = runSQL $ \c -> do
  i <- run c s v
  commit c
  return $ conv i
runUpdate (UpdateReturning s v conv) = runSQL $ \c -> do
  res <- conv <$> quickQuery' c s v
  commit c
  return res

runUpdate_ :: Update a -> IO ()
runUpdate_ u = () <$ runUpdate u

-- conversion helpers

type Converter a = [SqlValue] -> Maybe a

convertList :: Converter a -> [[SqlValue]] -> [a]
convertList f = catMaybes . map f

convertOne :: Converter a -> [[SqlValue]] -> Maybe a
convertOne f = listToMaybe . convertList f

--

toBool :: Converter Bool
toBool s = case s of
  [SqlBool b] -> Just b
  _           -> Nothing

fromBytea :: Converter B.ByteString
fromBytea s = case s of
  [bs@(SqlByteString _)] -> Just $ byteaUnpack bs
  _                      -> Nothing

-- | Convert binary `ByteString` to valid posgresql `SqlValue` (hex escaping)
byteaPack :: B.ByteString -> SqlValue
byteaPack bs = toSql $ "\\x" `B.append` B.concatMap packW8 bs
 where
   packW8 :: Word8 -> B.ByteString
   packW8 = B8.pack . printf "%02x"

-- | Retrieve a binary `ByteString` from a postgresql `SqlValue` (hex escaping)
byteaUnpack :: SqlValue -> B.ByteString
byteaUnpack sql = B.pack . unpackW8 $ fromSql sql
 where
  unpackW8 :: String -> [Word8]
  unpackW8 ('\\':'x':r) = unpackW8 r
  unpackW8 (a:b:c)      = case readHex [a,b] of
                               [(h,"")] -> (h:unpackW8 c)
                               _        -> error $ "invalid sequence: " ++ show [a,b]
  unpackW8 []           = []
  unpackW8 _            = error "uneven number of digits"

arrayPack :: [String] -> SqlValue
arrayPack = SqlString . combine . map escape
 where
  escape :: String -> String
  escape ('"':r)  = "\\\"" ++ escape r
  escape ('\\':r) = "\\\\" ++ escape r
  escape (c:r)    = c : escape r
  escape []       = []

  combine = surround "{" "}" . intercalate "," . map (surround "\"" "\"")

  surround l r s = l ++ s ++ r

-- | Support for postgres "array" type
arrayUnpack :: SqlValue -> Maybe [String]
arrayUnpack (SqlByteString s) = arrayUnpack (SqlString $ B8.unpack s)
arrayUnpack (SqlString s) = either (error . show) (Just) $
  parse postgresarray "postgres array parser" s
 where
  postgresarray :: Parser [String]
  postgresarray = char '{' *> arrayElem `sepBy` char ',' <* char '}'

  arrayElem = choice [ quoted, unquoted ]

  quoted = char '\"' *> quotedChar `manyTill` char '\"'

  quotedChar = choice [ char '\\' *> anyChar
                      , noneOf "\""
                      ]

  unquoted = many $ noneOf ",}"

arrayUnpack _ = Nothing
