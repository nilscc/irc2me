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
import Irc2me.Frontend.Messages.IrcMessage

type IrcConnections = Map AccountID (Map NetworkID IrcBroadcast)

------------------------------------------------------------------------------
-- Testing

testFormat :: (UTCTime, IrcMessage) -> String
testFormat (t, msg) =

  let time = show t -- formatTime defaultTimeLocale "%T" t

      cmd = (    msg ^? ircMessageType    . _Just . re _Show
             <|> msg ^? ircMessageTypeRaw . _Just . _Text
            ) ^. non "?"

      who = (    msg ^? ircFromUser   . _Just . userNick . _Text
             <|> msg ^? ircFromServer . _Just . _Text
            ) ^. non "-"

      par = msg ^. ircTo ^.. traversed . _Text & intercalate ", "

      cnt = (msg ^? ircContent . _Just . _Text) ^. non ""

  in "["  ++ time ++ "]"
  ++ " "  ++ cmd
  ++ " <" ++ who ++ ">"
  ++ " [" ++ par ++ "]"
  ++ " "  ++ cnt
