
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Util ((<>), debug) where

import Web.SocketIO.Type
import Web.SocketIO.Type.Log

import Data.Monoid ((<>))

import Control.Monad.Trans (liftIO, MonadIO)

debug :: (Functor m, MonadIO m, ConnectionLayer m) => Log -> m ()
debug log = do
    logLevel <- fmap logLevel getConfiguration
    if level <= logLevel then
        liftIO $ print log
    else
        return ()
    where   levelOf (Error _)   = 0
            levelOf (Warn  _)   = 1
            levelOf (Info  _)   = 2
            levelOf (Debug _)   = 3

            level = levelOf log