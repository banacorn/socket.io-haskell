{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Event where

import Web.SocketIO.Type
import Web.SocketIO.Type.Event
import Web.SocketIO.Session

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Chan.Lifted
import Data.IORef.Lifted

on :: Event -> CallbackM () -> SocketM ()
on event callback = do
    SocketM . lift . tell $ [(event, callback)]

(>~>) = on

reply :: CallbackM Reply
reply = ask

class Emittable m where
    emit :: Event -> Reply -> m ()
    (<~<) :: Event -> Reply -> m ()
    (<~<) = emit

instance Emittable SocketM where
    emit event reply = do
        channel <- ask
        writeChan channel (Emitter event reply)

instance Emittable CallbackM where
    emit event reply = do
        channel <- CallbackM . lift . lift $ ask
        writeChan channel (Emitter event reply)
