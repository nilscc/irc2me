module Irc2me.Backends.IRC.Helper where

import Control.Applicative
import Data.Time
import Data.List

import Data.Map (Map)

-- lens
import Control.Lens
import Data.Text.Lens

-- local
import Irc2me.Backends.IRC.Broadcast
import Irc2me.Database.Tables.Accounts
import Irc2me.Frontend.Messages.ChatMessage

type IrcConnections = Map AccountID (Map NetworkID IrcBroadcast)

------------------------------------------------------------------------------
-- Testing

testFormat :: (UTCTime, IrcMessage) -> String
testFormat (t, (msg, params)) =

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
