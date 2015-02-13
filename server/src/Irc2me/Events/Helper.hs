{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}

module Irc2me.Events.Helper where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Maybe

import Data.Maybe

-- lens
import Control.Lens hiding (Identity)

-- local
import Irc2me.Database.Tables.Networks
import Irc2me.Events.Types
import Irc2me.Frontend.Messages
import Irc2me.Backends.IRC.Helper

infix 4 ++=
(++=) :: MonadState s m => Setting' (->) s [a] -> [a] -> m ()
l ++= a = l %= (++ a)

at' :: (At m, AsEmpty (IxValue m)) => Index m -> Lens' m (IxValue m)
at' i = at i . non' _Empty

require :: MonadPlus m => Maybe a -> m a
require = maybe mzero return

choice :: Monad m => [MaybeT m ()] -> m Bool
choice l = do
  res <- runMaybeT $ msum l
  return $ isJust res

sendIrcMessage
  :: (MonadIO m, MonadReader AccountState m)
  => NetworkID
  -> ChatMessage
  -> m Bool
sendIrcMessage nid cm = do
  mc <- asks (^? connectedIrcNetworks . at nid . _Just . ircConnection)
  case mc of
    Nothing  -> return False
    Just con -> do
      liftIO $ con ^. ircSend $ cm
      return True

ircNetworkIdentity :: MonadReader AccountState m => NetworkID -> m (Maybe Identity)
ircNetworkIdentity nid = asks (^? connectedIrcNetworks . at nid . _Just . ircIdentity)

ircNetworkUser :: MonadReader AccountState m => NetworkID -> m (Maybe User)
ircNetworkUser nid = do
  mident <- ircNetworkIdentity nid
  case mident of
    Just ident | Just nick <- ident ^. identityNick ->
      return . Just $ emptyUser & userNick .~ nick
    _ ->
      return Nothing
