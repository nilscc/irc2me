{-# LANGUAGE TemplateHaskell #-}

module Irc2me.Backends.IRC.NetworkState where

import Control.Monad.Reader

import Data.Text

-- lens
import Control.Lens hiding (Identity)

-- stm
import Control.Concurrent.STM

-- local
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks
import Irc2me.Frontend.Messages

data NetworkState = NetworkState
  { _nsAccountID :: AccountID
  , _nsNetworkID :: NetworkID
  , _nsIdentity  :: TVar Identity
  }

makeLenses ''NetworkState

newNetworkState
  :: MonadIO m
  => AccountID
  -> NetworkID
  -> Identity
  -> m NetworkState
newNetworkState aid nid ident = do
  tIdent <- liftIO $ newTVarIO ident
  return $ NetworkState aid nid tIdent

getNetworkIdentitiy :: MonadIO m => NetworkState -> m Identity
getNetworkIdentitiy ns = do
  liftIO $ readTVarIO $ ns ^. nsIdentity

--------------------------------------------------------------------------------
-- Guards & such

whenSentToMe :: Monad m => Identity -> Parameters -> m () -> m ()
whenSentToMe ident params go = do

  let nick :: Maybe Text
      nick = ident ^? identityNick . _Just

      firstParam = params ^? ix 0

  when (nick == firstParam) go
