{-# LANGUAGE TemplateHaskell #-}

module Irc2me.Backends.IRC.NetworkState where

import Control.Monad.Trans

-- lens
import Control.Lens hiding (Identity)

-- stm
import Control.Concurrent.STM

-- local
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks
import Irc2me.Frontend.Messages

type NetworkState = TVar NetworkStateData

data NetworkStateData = NetworkState
  { _nsAccountID :: AccountID
  , _nsNetworkID :: NetworkID
  , _nsIdentity  :: Identity
  }

makeLenses ''NetworkStateData

newNetworkState
  :: MonadIO m
  => AccountID
  -> NetworkID
  -> Identity
  -> m NetworkState
newNetworkState aid nid ident = do
  liftIO $ newTVarIO $ NetworkState aid nid ident
