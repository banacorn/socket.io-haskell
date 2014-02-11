{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Util ((<>), debugLog, debugSession, debug) where

--------------------------------------------------------------------------------
import Control.Concurrent.Chan
import Control.Monad.Trans 					(liftIO, MonadIO)
--------------------------------------------------------------------------------
import Web.SocketIO.Types

--------------------------------------------------------------------------------
debug :: (Functor m, MonadIO m, ConnectionLayer m) => (ByteString -> Log) -> ByteString -> m ()
debug logType message = do
    logLevel' <- fmap logLevel getConfiguration
    stdout' <- fmap envStdout getEnv
    if level <= logLevel' then liftIO $ writeChan stdout' (serialize log') else return ()
    where   log' = logType message
            levelOf (Error _) = 0
            levelOf (Warn  _) = 1
            levelOf (Info  _) = 2
            levelOf (Debug _) = 3
            level = levelOf log' 

--------------------------------------------------------------------------------
debugLog :: (Functor m, MonadIO m, ConnectionLayer m) => (ByteString -> Log) -> Session -> ByteString -> m ()
debugLog logType (Session sessionID _ _ _ _) message = debug logType (sessionID <> "    " <> serialize message) 

--------------------------------------------------------------------------------
debugSession :: (Functor m, MonadIO m, ConnectionLayer m, SessionLayer m) => (ByteString -> Log) -> ByteString -> m ()
debugSession logType message = do
    Session sessionID _ _ _ _ <- getSession
    debug logType $ fromByteString (sessionID <> "    " <> message)