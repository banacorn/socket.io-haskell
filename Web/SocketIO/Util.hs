
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Util ((<>), debug) where

import Web.SocketIO.Type
import Web.SocketIO.Type.Log

import Data.Monoid ((<>))

import Control.Monad.Trans (liftIO, MonadIO)

debug :: (MonadIO m, ConnectionLayer m) => Log -> m ()
debug log = do
    config <- getConfiguration
    liftIO $ print config
