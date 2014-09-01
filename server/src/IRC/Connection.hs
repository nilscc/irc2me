{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IRC.Connection
  ( -- * Connecting to IRC
    connect
  , TLSSettings(..)
  , Connection
  , ircMessages, IrcProducer
  , sendIrc
  , closeConnection
    -- ** Utilities
  , sendIrcT
  , handleIrcMessages
  , module IRC.Message.Filter
    -- * Exceptions
  , ConnectException(..)
    -- ** Haltes producers
  , HaltedProducer, continue
  ) where

import Control.Applicative
import Control.Exception

import Control.Monad.State

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL

import Data.Time

import Network
import System.IO

-- tls
import Network.TLS

-- attoparsec
import Data.Attoparsec.ByteString.Char8 as Attoparsec

-- irc-bytestring
import Network.IRC.ByteString.Parser

-- pipes
import Pipes
import Pipes.Parse      as Pipes
import Pipes.Attoparsec as Pipes

-- local pipe modules
import Network.TLS.Pipes
import System.IO.Pipes

-- local
import IRC.Types
import IRC.TLS
import IRC.Message.Filter


------------------------------------------------------------------------------
-- Connection

type IrcProducer m = Producer (UTCTime, IRCMsg) m (Maybe (ConnectException m))

data Connection m

  = PlaintextConnection
    { ircMessages :: IrcProducer m
    , _handle     :: Handle
    }

  | TLSConnection
    { ircMessages :: IrcProducer m
    , _context    :: Context
    }

toConnection :: MonadIO m => Handle -> IrcProducer m -> Connection m
toConnection h p = PlaintextConnection p h

toTLSConnection :: MonadIO m => Context -> IrcProducer m -> Connection m
toTLSConnection ctxt p = TLSConnection p ctxt

connect
  :: (MonadIO m, Functor m)
  => TLSSettings
  -> HostName
  -> PortID
  -> m (Maybe (Connection m))
  -- -> Producer (UTCTime, IRCMsg) m (Maybe (ConnectException m))
connect Plaintext hostname port = do
  h <- liftIO $ connectTo hostname port
  return $ Just $ toConnection h $ continueWith (fromHandle h)

connect TLS hostname port = do
  h <- liftIO $ connectTo hostname port
  mp <- fromTLS h (clientParams hostname)
  return $ case mp of
    Nothing        -> Nothing
    Just (p, ctxt) -> Just $ toTLSConnection ctxt $ continueWith p

connect STARTTLS hostname port = do
  h <- liftIO $ connectTo hostname port
  runStarttls hostname h

connect OptionalTLS hostname port = do
  h <- liftIO $ connectTo hostname port

  -- send CAP
  liftIO $ B8.hPutStr h "CAP"

  do
    -- wait for reponse
    (prod, r, msgs) <- capLoop (fromHandle h) []
    case r of

      -- run STARTTLS
      Just cap | "tls" `elem` B8.words cap ->
        runStarttls hostname h

      -- continue plaintext/without TLS
      _ -> return $ Just $ toConnection h $ mapM_ yield msgs >> continueWith prod

-- | Send \"STARTTLS\" to server and wait for \"670\" response.
runStarttls
  :: MonadIO m
  => HostName -> Handle
  -> m (Maybe (Connection m))
runStarttls hostname h = do

  liftIO $ B8.hPutStrLn h "STARTTLS"

  (success,msgs) <- starttlsLoop (fromHandle h) []

  if success then do
    mp <- fromTLS h (clientParams hostname)
    case mp of
      Just (p, ctxt) ->
        return $ Just $ toTLSConnection ctxt $ mapM_ yield msgs >> continueWith p
      Nothing -> return Nothing
   else
    return $ Nothing --return $ Just STARTTLSFailed

------------------------------------------------------------------------------
-- Response loops

capLoop
  :: MonadIO m
  => Producer ByteString m e
  -> [(UTCTime, IRCMsg)]
  -> m (Producer ByteString m e, Maybe ByteString, [(UTCTime, IRCMsg)])
capLoop prod msgs = do
  (r, prod') <- Pipes.runStateT (Pipes.parse ircParser) prod
  case r of
    Just (Right msg)

      -- successful response
      | msg `hasCommand` "CAP" -> return $ (prod', Just (msgTrail msg), msgs)

      -- ignore/re-yield NOTICE and 020 messages and loop
      | msg `hasCommand` "020" || msg `hasCommand` "NOTICE" -> do

        now <- liftIO $ getCurrentTime
        capLoop prod' (msgs ++ [(now,msg)])

      -- yield everything else and quit, returning the rest of the producer
      | otherwise -> do

        now <- liftIO $ getCurrentTime
        return (prod', Nothing, msgs ++ [(now,msg)])

    _ -> return (prod', Nothing, msgs)


starttlsLoop
  :: MonadIO m
  => Producer ByteString m e
  -> [(UTCTime, IRCMsg)]
  -> m (Bool, [(UTCTime, IRCMsg)])
starttlsLoop prod msgs = do
  (r, prod') <- Pipes.runStateT (Pipes.parse ircParser) prod
  case r of
    Just (Right msg)

      -- STARTTLS accepted
      | msg `hasCommand` "670" -> return (True, msgs)

      -- forward NOTICE messages
      | msg `hasCommand` "NOTICE" -> do
        now <- liftIO $ getCurrentTime
        starttlsLoop prod' (msgs ++[(now, msg)])

    _ -> return (False, msgs)


------------------------------------------------------------------------------
-- Exceptions

newtype HaltedProducer m = HaltedProducer
  { runHaltedProducer :: IrcProducer m
  }

continue :: Connection m -> HaltedProducer m -> Connection m
continue con hp = con { ircMessages = runHaltedProducer hp }

data ConnectException m
  = TLSFailed
  | STARTTLSFailed
  | TLSException'   TLSException
  | IOException'    IOException
  | ParsingError'   ParsingError  (HaltedProducer m)

instance Show (ConnectException m) where
  show TLSFailed           = "TLS failed"
  show STARTTLSFailed      = "STARTTLS failed"
  show (TLSException' e)   = "TLS exception: " ++ show e
  show (IOException'  e)   = "IO exception: " ++ show e
  show (ParsingError' e _) = "Parsing error: " ++ show e

class IsConnectException m e where

  toConnectException :: e -> ConnectException m

instance IsConnectException m IOException where
  toConnectException = IOException'

instance IsConnectException m (Either TLSException IOException) where
  toConnectException (Left  e) = TLSException' e
  toConnectException (Right e) = IOException'  e


------------------------------------------------------------------------------
-- Parsers & parsing producers

parsedIrcMessage
  :: MonadIO m
  => Producer ByteString m a
  -> Producer (UTCTime, IRCMsg)
              m
              (Either (ParsingError, Producer ByteString m a) a)
parsedIrcMessage bsprod = do
  for (parsed ircParser bsprod) addTimeStamp
 where
  addTimeStamp msg = do
    now <- lift . liftIO $ getCurrentTime
    yield (now, msg)

ircParser :: Attoparsec.Parser IRCMsg
ircParser = skipMany space *> ircLine

-- | Continue parsing IRC messages from a `Producer`, possibly returned
-- from a `ParsingError'`
continueWith
  :: (MonadIO m, IsConnectException m e)
  => Producer ByteString m (Maybe e)
  -> IrcProducer m
continueWith p = do
  r <- parsedIrcMessage p
  return $ case r of
    Left (pe, pr)  -> Just $ ParsingError' pe (HaltedProducer $ continueWith pr)
    Right (Just e) -> Just $ toConnectException e
    Right Nothing  -> Nothing


------------------------------------------------------------------------------
-- Sending

sendIrc :: MonadIO m => Connection m -> IRCMsg -> m ()
sendIrc con msg = liftIO $ mkSafe $ case con of
  PlaintextConnection _ h -> B8.hPutStrLn h bs
  TLSConnection       _ c -> sendData     c $ BL.fromStrict bs
 where
  mkSafe io = io `catch` (\(_ :: IOException) -> return ())
  bs = fromIRCMsg msg

------------------------------------------------------------------------------
-- Close a connection

closeConnection :: MonadIO m => Connection m -> m ()
closeConnection con = liftIO $ case con of
  PlaintextConnection _ h -> hClose h
  TLSConnection       _ c -> do
    bye c
    backendClose $ ctxConnection c

------------------------------------------------------------------------------
-- Utility

-- | Handle incoming IRC messages. This consumes all incoming messages of the
-- current connection.
handleIrcMessages
  :: MonadIO m
  => Connection m
  -> ((UTCTime, IRCMsg) -> m ())
  -> m (Maybe (ConnectException m))
handleIrcMessages con f =

  runEffect $ for (ircMessages con) (lift . f)

-- | `send` lifted to the `IrcT` monad
sendIrcT :: MonadIO m => Connection m -> IRCMsg -> IrcT m ()
sendIrcT con msg = lift . lift $ sendIrc con msg
