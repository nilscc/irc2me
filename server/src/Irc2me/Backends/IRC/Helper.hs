{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- disable orphan warning for (ChatMessage -> IO ()) show instance
{-# OPTIONS -fno-warn-orphans #-}

module Irc2me.Backends.IRC.Helper where

--import Control.Concurrent
import Control.Lens (Iso', iso)
import Data.Time
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

--import Data.Map (Map)

-- lens
--import Control.Lens hiding (Identity)
--import Data.Text.Lens

-- irc-bytestring
import Network.IRC.ByteString.Parser as IRC

-- local
--import Irc2me.Database.Tables.Accounts
--import Irc2me.Database.Tables.Networks
--import Irc2me.Frontend.Messages.ChatMessage

encoded :: Iso' ByteString Text
encoded = iso decodeUtf8 encodeUtf8

{-
instance Show (ChatMessage -> IO ()) where
  show _ = "(ChatMessage -> IO ())"

data IrcConnection = IrcConnection
  { _ircThread    :: ThreadId
  , _ircSend      :: ChatMessage -> IO ()
  }
  deriving Show

instance Eq IrcConnection where
  c1 == c2 = _ircThread c1 == _ircThread c2

makeLenses ''IrcConnection

type IrcConnections = Map AccountID (Map NetworkID IrcConnection)
-}

------------------------------------------------------------------------------
-- Testing

testFormat :: (UTCTime, IRCMsg) -> String
testFormat (t, msg) = "[" ++ show t ++ "] " ++ show msg

{-
  let time = show t -- formatTime defaultTimeLocale "%T" t

      cmd = (    msg ^? messageType      . _Just . re _Show
             <|> msg ^? messageTypeOther . _Just . _Text
            ) ^. non "?"

      who = (    msg ^? messageFromUser   . _Just . userNick . _Text
             <|> msg ^? messageFromServer . _Just . _Text
            ) ^. non "-"

      par = params ^.. traversed . _Text & intercalate ", "

      cnt = (msg ^? messageContent . _Just . _Text) ^. non ""

  in "["  ++ time ++ "]"
  ++ " "  ++ cmd
  ++ " <" ++ who ++ ">"
  ++ " [" ++ par ++ "]"
  ++ " "  ++ cnt
-}
