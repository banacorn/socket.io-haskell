{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Util ((<>), debug) where

--------------------------------------------------------------------------------
import Control.Concurrent.Chan
import Control.Monad.Trans 					(liftIO, MonadIO)
--------------------------------------------------------------------------------
import Web.SocketIO.Types

--------------------------------------------------------------------------------
debug :: (Functor m, MonadIO m, ConnectionLayer m) => Log -> m ()
debug message = do
    logLevel' <- fmap logLevel getConfiguration
    stdout' <- fmap envStdout getEnv
    if level <= logLevel' then liftIO $ writeChan stdout' (show message) else return ()
    where   levelOf (Error _)   = 0
            levelOf (Warn  _)   = 1
            levelOf (Info  _)   = 2
            levelOf (Debug _)   = 3

            level = levelOf message