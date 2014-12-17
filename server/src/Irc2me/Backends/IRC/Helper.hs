module Irc2me.Backends.IRC.Helper where

import Control.Concurrent
import Data.Time

import Data.Map (Map)

-- lens
--import Control.Lens
--import Data.Text.Lens

-- irc-bytestring
import Network.IRC.ByteString.Parser as IRC

-- local
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks

data IrcConnection = IrcConnection
  { broadcastThread :: ThreadId
  }
  deriving (Eq, Show)

type IrcConnections = Map AccountID (Map NetworkID IrcConnection)

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
