{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

module Irc2me.Backends.IRC.NetworkState where

import Control.Applicative
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as Text

-- lens
import Control.Lens hiding (Identity)

-- stm
import Control.Concurrent.STM

-- local
import Irc2me.Database.Tables.Accounts
import Irc2me.Database.Tables.Networks
import Irc2me.Frontend.Messages

type Channelname = Text
type Nickname    = Text

data NetworkState = NetworkState
  { _nsAccountID :: AccountID
  , _nsNetworkID :: NetworkID
  , _nsIdentity  :: TVar Identity
  , _nsChannels  :: TVar (Map Channelname ChannelState)
  }
  deriving (Eq)

data ChannelContext
  = ChannelNONE
  | ChannelUSERLIST
  deriving (Eq, Show)

data ChannelState = ChannelState
  { _csUsers    :: Map Nickname (Maybe Userflag)
  , _csContext  :: ChannelContext
  }
  deriving (Eq, Show)

makeLenses ''NetworkState
makeLenses ''ChannelState

newNetworkState
  :: MonadIO m
  => AccountID
  -> NetworkID
  -> Identity
  -> m NetworkState
newNetworkState aid nid ident = do
  tIdent <- liftIO $ newTVarIO ident
  tChan  <- liftIO $ newTVarIO M.empty
  return $ NetworkState aid nid tIdent tChan

getNetworkIdentitiy :: MonadIO m => NetworkState -> m Identity
getNetworkIdentitiy ns = do
  liftIO $ readTVarIO $ ns ^. nsIdentity

getUserlist :: MonadIO m => NetworkState -> Channelname -> m (Maybe (Map Nickname (Maybe Userflag)))
getUserlist ns chan = do
  liftIO $ atomically $ do
    (fmap (view csUsers) . M.lookup chan) <$> readTVar (ns ^. nsChannels)

type OldNickname = Nickname
type NewNickname = Nickname

------------------------------------------------------------------------------
-- Handle IRC events

changeNickname
  :: MonadIO m => NetworkState -> OldNickname -> NewNickname -> m ()
changeNickname ns old new = do
  liftIO $ atomically $ do

    ident <- readTVar (ns ^. nsIdentity)
    if ident ^. identityNick == Just old then do
      -- change own nick
      writeTVar (ns ^. nsIdentity) (ident & identityNick .~ Just new)
     else do
      -- update nick in all channels
      modifyTVar (ns ^. nsChannels) $ \chans -> 
        M.map `flip` chans $ \chan -> chan & csUsers .~
          let usrs = chan ^. csUsers in
          case M.lookup old usrs of
            Just userdata -> M.insert new userdata $ M.delete old usrs
            _             -> usrs -- do nothing

addUserlist
  :: MonadIO m => NetworkState -> Channelname -> [Nickname] -> m ()
addUserlist ns chan nicks = do
  liftIO $ atomically $ do
    modifyTVar (ns ^. nsChannels) $ \chans ->
      M.alter f chan chans
 where

  f (Just cs)

    -- currently in NAMREPLY, append userlist:
    | ChannelUSERLIST <- cs ^. csContext
    = Just $ cs & csUsers .~ M.union (cs ^. csUsers) userlistAsMap

    -- no current context -> replace userlist
    | otherwise 
    = Just $ cs & csUsers   .~ userlistAsMap
                & csContext .~ ChannelUSERLIST -- new context

  -- new channel -> create new channel state
  f Nothing = Just $ ChannelState
    { _csUsers   = userlistAsMap
    , _csContext = ChannelUSERLIST
    }

  userlistAsMap :: Map Nickname (Maybe Userflag)
  userlistAsMap = M.fromList $ map `flip` nicks $ \nick ->
    case Text.uncons nick of
      Just ('@',nick') -> (nick', Just Operator)
      Just ('+',nick') -> (nick', Just Voice)
      _                -> (nick , Nothing)

endUserlist
  :: MonadIO m => NetworkState -> Channelname -> m ()
endUserlist ns chan = do
  liftIO $ atomically $ do
    modifyTVar (ns ^. nsChannels) $ \chans ->
      M.adjust f chan chans
 where

  f cs

    | ChannelUSERLIST <- cs ^. csContext
    = cs & csContext .~ ChannelNONE

    | otherwise = cs -- do nothing

--------------------------------------------------------------------------------
-- Guards & such

whenSentToMe :: Monad m => Identity -> Parameters -> m () -> m ()
whenSentToMe ident params go = do

  let nick :: Maybe Text
      nick = ident ^? identityNick . _Just

      firstParam = params ^? ix 0

  when (nick == firstParam) go
