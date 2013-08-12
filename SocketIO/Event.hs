{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Event where

import SocketIO.Type
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity


on :: Event -> Handler -> SocketM ()
on event handler = modify $ register event handler

register :: Event -> Handler -> EventMap -> EventMap
register event handler eventMap = Map.insertWith (++) event [handler] eventMap

extract :: SocketM () -> EventMap
extract = runIdentity . flip execStateT Map.empty . runWriterT . runSocketM

trigger :: EventMap -> Event -> Reply -> IO ()
trigger eventMap event reply = case Map.lookup event eventMap of
    Just handlers   -> mapM_ (\h -> h reply) handlers
    Nothing         -> return ()