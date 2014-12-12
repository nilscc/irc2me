{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Network.IRC.Connection
  ( -- * Connecting to IRC
    connect
  , TLSSettings(..)
  , Connection
  , ircMessages, IrcProducer
  , sendIrc
  , closeConnection
    -- * Exceptions
  , ConnectionException(..)
    -- ** Haltes producers
  , HaltedProducer, continue
    -- * Utilities
  , sendIrcT
  , handleIrcMessages
  , module Network.IRC.Message.Filter
  , dumpHaltedProducer
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Except

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL

import Data.Time

import Network
import System.IO

-- tls
import Network.TLS

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
import Network.IRC.TLS
import Network.IRC.Message.Filter


------------------------------------------------------------------------------
-- Connection

type IrcProducer m = Producer (UTCTime, IRCMsg) m (Maybe (ConnectionException m))

--
-- Connection data type
--

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

--
-- Starting a connection
--

data TLSSettings
  = TLS         -- ^ Use TLS
  | STARTTLS    -- ^ Use TLS via STARTTLS
  | OptionalTLS -- ^ Use CAP command to find out whether or not IRC server
                --   supports TLS
  | Plaintext   -- ^ No TLS at all
  deriving Show

connect
  :: MonadIO m
  => TLSSettings
  -> HostName
  -> PortID
  -> m (Either (ConnectionException m) (Connection m))
connect tls hostname port = runExceptT $ do

  -- open connection to host
  h <- lift . liftIO $ connectTo hostname port

  -- setup TLS according to settings
  case tls of

    Plaintext -> do

      -- not much to do here:
      return $ toConnection h $ continueWith (fromHandle h)

    TLS -> do

      -- start TLS handshake
      cpams <- liftIO $ initClientParams hostname
      mp <- lift $ fromTLS h cpams
      case mp of
        Right (p, ctxt) -> return $ toTLSConnection ctxt $ continueWith p
        Left (Left  e) -> throwError $ TLSException' e
        Left (Right e) -> throwError $ IOException' e

    STARTTLS -> do

      runStarttls hostname h

    OptionalTLS -> do

      -- send CAP and wait for reponse
      liftIO $ B8.hPutStr h "CAP"
      (prod, r, msgs) <- lift $ capLoop (fromHandle h) []

      case r of

        -- TLS supported
        Just cap | "tls" `elem` B8.words cap ->
          runStarttls hostname h

        -- plaintext/without TLS
        _ ->
          return $ toConnection h $ mapM_ yield msgs >> continueWith prod


------------------------------------------------------------------------------
-- Response loops

-- | Send \"STARTTLS\" to server and wait for \"670\" response.
runStarttls
  :: MonadIO m
  => HostName -> Handle
  -> ExceptT (ConnectionException m) m (Connection m)
runStarttls hostname h = do

  liftIO $ B8.hPutStrLn h "STARTTLS"

  (success,msgs) <- starttlsLoop (fromHandle h) []

  if success then do
    cpams <- liftIO $ initClientParams hostname
    mp <- lift $ fromTLS h cpams
    case mp of
      Right (p, ctxt) ->
        return $ toTLSConnection ctxt $ mapM_ yield msgs >> continueWith p
      Left (Left tls) -> throwError $ TLSException' tls
      Left (Right io) -> throwError $ IOException' io
   else
    throwError STARTTLSFailed

-- | After sending STARTTLS wait for '670' response on the encrypted connection
starttlsLoop
  :: MonadIO m
  => Producer ByteString m e
  -> [(UTCTime, IRCMsg)]
  -> m (Bool, [(UTCTime, IRCMsg)])
starttlsLoop prod msgs = do
  (r, prod') <- Pipes.runStateT (Pipes.parse ircLine) prod
  case r of
    Just (Right msg)

      -- STARTTLS accepted
      | msg `hasCommand` "670" -> return (True, msgs)

      -- forward NOTICE messages
      | msg `hasCommand` "NOTICE" -> do
        now <- liftIO $ getCurrentTime
        starttlsLoop prod' (msgs ++[(now, msg)])

    _ -> return (False, msgs)

-- | Wait for CAP messages with server's capabilities
capLoop
  :: MonadIO m
  => Producer ByteString m e
  -> [(UTCTime, IRCMsg)]
  -> m (Producer ByteString m e, Maybe ByteString, [(UTCTime, IRCMsg)])
capLoop prod msgs = do
  (r, prod') <- Pipes.runStateT (Pipes.parse ircLine) prod
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


------------------------------------------------------------------------------
-- Exceptions

data HaltedProducer m where
  HaltedProducer :: IsConnectionException m e => Producer ByteString m (Maybe e) -> HaltedProducer m

continue :: MonadIO m => Connection m -> HaltedProducer m -> Connection m
continue con (HaltedProducer hp) = con { ircMessages = continueWith hp }

data ConnectionException m
  = TLSFailed
  | STARTTLSFailed
  | TLSException'   TLSException
  | IOException'    IOException
  | ParsingError'   ParsingError  (HaltedProducer m)

instance Show (ConnectionException m) where
  show TLSFailed           = "TLS failed"
  show STARTTLSFailed      = "STARTTLS failed"
  show (TLSException' e)   = "TLS exception: " ++ show e
  show (IOException'  e)   = "IO exception: " ++ show e
  show (ParsingError' e _) = "Parsing error: " ++ show e

class IsConnectionException m e where

  toConnectionException :: e -> ConnectionException m

instance IsConnectionException m IOException where
  toConnectionException = IOException'

instance IsConnectionException m (Either TLSException IOException) where
  toConnectionException (Left  e) = TLSException' e
  toConnectionException (Right e) = IOException'  e


------------------------------------------------------------------------------
-- Parsers & parsing producers

parsedIrcMessage
  :: MonadIO m
  => Producer ByteString m a
  -> Producer (UTCTime, IRCMsg)
              m
              (Either (ParsingError, Producer ByteString m a) a)
parsedIrcMessage bsprod = do
  for (parsed ircLine bsprod) addTimeStamp
 where
  addTimeStamp msg = do
    now <- lift . liftIO $ getCurrentTime
    yield (now, msg)

-- | Continue parsing IRC messages from a `Producer`, possibly returned
-- from a `ParsingError'`
continueWith
  :: (MonadIO m, IsConnectionException m e)
  => Producer ByteString m (Maybe e)
  -> IrcProducer m
continueWith p = do
  r <- parsedIrcMessage p
  return $ case r of
    Left (pe, pr)  -> Just $ ParsingError' pe (HaltedProducer pr)
    Right (Just e) -> Just $ toConnectionException e
    Right Nothing  -> Nothing


------------------------------------------------------------------------------
-- Sending

sendIrc :: Connection m -> IRCMsg -> IO ()
sendIrc con msg = mkSafe $ case con of
  PlaintextConnection _ h -> B8.hPutStrLn h bs
  TLSConnection       _ c -> sendData     c $ BL.fromStrict bs
 where
  mkSafe io = io `catch` (\(_ :: IOException) -> return ())
  bs = fromIRCMsg msg


------------------------------------------------------------------------------
-- Close a connection

closeConnection :: Connection m -> IO ()
closeConnection con = mkSafe $ case con of
  PlaintextConnection _ h -> hClose h
  TLSConnection       _ c -> do
    bye c
    backendClose $ ctxConnection c
 where
  mkSafe io = io `catch` (\(_ :: IOException) -> return ())


------------------------------------------------------------------------------
-- Utility

-- | Handle incoming IRC messages. This consumes all incoming messages of the
-- current connection.
handleIrcMessages
  :: MonadIO m
  => Connection m
  -> ((UTCTime, IRCMsg) -> m ())
  -> m (Maybe (ConnectionException m))
handleIrcMessages con f =

  runEffect $ for (ircMessages con) (lift . f)

-- | `send` lifted to the `IrcT` monad
sendIrcT :: MonadIO m => Connection m -> IRCMsg -> IrcT m ()
sendIrcT con msg = lift . lift . liftIO $ sendIrc con msg

dumpHaltedProducer :: Monad m => HaltedProducer m -> m ByteString
dumpHaltedProducer (HaltedProducer p) = do
  runEffect $ ("" <$ p) >-> await
