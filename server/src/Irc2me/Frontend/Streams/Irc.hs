module Irc2me.Frontend.Streams.Irc where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- lenses
import Control.Lens

-- local
import Control.Concurrent.Event
import Irc2me.Events

import Irc2me.Database.Tables.Networks

import Irc2me.Frontend.Messages.Client
import Irc2me.Frontend.Messages.Server
import Irc2me.Frontend.Messages.IrcNetwork
import Irc2me.Frontend.Messages.IrcMessage

import Irc2me.Frontend.Streams.Helper
import Irc2me.Frontend.Streams.StreamT

ircStream :: ServerResponse
ircStream = choice

  [ do
      -- receive all messages from the 'networks' field
      networks <- foldROn clientNetworks messageFold

      -- make sure we have any messages to send
      guard $ not . all null $ map snd networks

      -- send messages to IRC network
      withAccount $ \accid ->
        forM_ networks $ \(netid, msgs) -> do
          forM_ msgs $ \msg -> do
            liftIO $ putStrLn $ "[" ++ show netid ++ "] " ++ show (msg ^. ircContent)
            raiseEvent $ sendIrcMessage accid (NetworkID $ fromIntegral netid) msg

      return responseOkMessage
  ]
 where
  messageFold = (,) <$> Fold (networkId . _Just)
                    <*> Fold networkMessages
