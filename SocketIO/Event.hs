{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Event where

import SocketIO.Type
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Chan.Lifted
import Data.IORef.Lifted

on :: Event -> CallbackM () -> SocketM ()
on event callback = do
    SocketM . lift . tell $ [(event, callback)]

(>~>) = on

emit :: Event -> Reply -> SocketM ()
emit event reply = do
    chan <- ask
    writeChan chan $ Emitter event reply

(<~<) = emit

executeHandler :: SocketM () -> Buffer -> SessionM [Listener]
executeHandler handler channel = liftIO $ execWriterT (runReaderT (runSocketM handler) channel)

registerListener :: [Listener] -> SessionM ()
registerListener listeners = ask >>= flip writeIORef listeners . getListener

triggerListener :: Emitter -> SessionM ()
triggerListener (Emitter event reply) = do
    -- read
    listeners <- ask >>= readIORef . getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> do
        liftIO $ runReaderT (execWriterT (runCallbackM callback)) reply