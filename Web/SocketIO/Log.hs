--------------------------------------------------------------------------------
-- | Exports some logging utilities.
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Log ((<>), logRaw, logWithSession, logWithSessionID, showStatusBar) where

--------------------------------------------------------------------------------
import Web.SocketIO.Types

--------------------------------------------------------------------------------
import Control.Concurrent.Chan
import Control.Monad.Trans                  (liftIO, MonadIO)
    
--------------------------------------------------------------------------------
-- | Write log to channel according to log level and configurations
logRaw :: (Functor m, MonadIO m, ConnectionLayer m) => (ByteString -> Log) -> ByteString -> m ()
logRaw logType message = do
    logLevel' <- fmap logLevel getConfiguration
    logChannel <- fmap envLogChannel getEnv
    if level <= logLevel' then liftIO $ writeChan logChannel (serialize log') else return ()
    where   log' = logType message
            levelOf (Error _) = 0
            levelOf (Warn  _) = 1
            levelOf (Info  _) = 2
            levelOf (Debug _) = 3
            level = levelOf log' 

--------------------------------------------------------------------------------
-- | Attaches `Web.SocketIO.Types.Base.SessionID`
logWithSessionID :: (Functor m, MonadIO m, ConnectionLayer m) => (ByteString -> Log) -> SessionID -> ByteString -> m ()
logWithSessionID logType sessionID message = logRaw logType (sessionID <> "    " <> serialize message) 

--------------------------------------------------------------------------------
-- | Attaches `Web.SocketIO.Types.Base.SessionID` automatically
logWithSession :: (Functor m, MonadIO m, ConnectionLayer m, SessionLayer m) => (ByteString -> Log) -> ByteString -> m ()
logWithSession logType message = do
    Session sessionID _ _ _ _ <- getSession
    logRaw logType $ fromByteString (sessionID <> "    " <> message)

--------------------------------------------------------------------------------
-- | Show status bar
showStatusBar :: IO ()
showStatusBar = return ()