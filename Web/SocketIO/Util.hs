--------------------------------------------------------------------------------
-- | Exports some logging utilities and other useful functions
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Util ((<>), debugLog, debugSession, debug, showStatusBar) where

--------------------------------------------------------------------------------
import Web.SocketIO.Types

--------------------------------------------------------------------------------
import Control.Concurrent.Chan
import Control.Monad.Trans                  (liftIO, MonadIO)
    
--------------------------------------------------------------------------------
-- | Write log to channel according to log level and configurations
debug :: (Functor m, MonadIO m, ConnectionLayer m) => (ByteString -> Log) -> ByteString -> m ()
debug logType message = do
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
debugLog :: (Functor m, MonadIO m, ConnectionLayer m) => (ByteString -> Log) -> Session -> ByteString -> m ()
debugLog logType (Session sessionID _ _ _ _) message = debug logType (sessionID <> "    " <> serialize message) 

--------------------------------------------------------------------------------
-- | Attaches `Web.SocketIO.Types.Base.SessionID` automatically
debugSession :: (Functor m, MonadIO m, ConnectionLayer m, SessionLayer m) => (ByteString -> Log) -> ByteString -> m ()
debugSession logType message = do
    Session sessionID _ _ _ _ <- getSession
    debug logType $ fromByteString (sessionID <> "    " <> message)

--------------------------------------------------------------------------------
-- | Show status bar
showStatusBar :: IO ()
showStatusBar = return ()