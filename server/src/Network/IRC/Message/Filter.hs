{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message.Filter where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.ByteString (ByteString)
import Data.Time

-- irc-bytestring
import Network.IRC.ByteString.Parser

type IrcT m = MaybeT (ReaderT (UTCTime, IRCMsg) m)

withMsg :: Monad m => (UTCTime, IRCMsg) -> [IrcT m a] -> m (Maybe a)
withMsg tmsg what = runReaderT (runMaybeT $ msum what) tmsg

withMsg_ :: Monad m => (UTCTime, IRCMsg) -> [IrcT m ()] -> m ()
withMsg_ tmsg what = do
  _ <- withMsg tmsg what
  return ()

--
-- Filters
--

hasCommand :: IRCMsg -> ByteString -> Bool
hasCommand msg c = c == msgCmd msg

command :: Monad m => ByteString -> IrcT m ()
command c = do
  (_,msg) <- ask
  guard $ msg `hasCommand` c

privMsg :: MonadIO m => (ByteString -> Bool) -> IrcT m ()
privMsg t = do
  (_,msg) <- ask
  guard $ msg `hasCommand` "PRIVMSG" && t (msgTrail msg)

serverMessage :: Monad m => IrcT m ServerName
serverMessage = do
  (_,msg) <- ask
  case msgPrefix msg of
    Just (Right s) -> return s
    _              -> mzero

userMessage :: Monad m => IrcT m UserInfo
userMessage = do
  (_,msg) <- ask
  case msgPrefix msg of
    Just (Left ui) -> return ui
    _              -> mzero
