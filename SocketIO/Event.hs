{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Event where

import SocketIO.Type
import Control.Monad.Writer

on :: Event -> CallbackM () -> SocketM ()
on event callback = do
    SocketM . lift . tell $ [(event, callback)]

(>~>) = on

emit :: Event -> Reply -> SocketM ()
emit event reply = tell [Emitter event reply]

(<~<) = emit

extractListener :: SocketM () -> IO [Listener]
extractListener = execWriterT . execWriterT . runSocketM

extractEmitter :: SocketM () -> IO [Emitter]
extractEmitter = fmap fst . runWriterT . execWriterT . runSocketM

--trigger :: EventMap -> Event -> Reply -> IO ()
--trigger eventMap event reply = case Map.lookup event eventMap of
--    Just handlers   -> mapM_ (\h -> h reply) handlers
--    Nothing         -> return ()