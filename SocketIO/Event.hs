{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Event where

import SocketIO.Type
import qualified Data.Map as Map

on :: Event -> Handler -> SocketM ()
on event handler = modify $ register event handler

register :: Event -> Handler -> EventMap -> EventMap
register event handler eventMap = Map.insertWith (++) event [handler] eventMap

extract :: SocketM () -> EventMap
extract = runIdentity . flip execStateT Map.empty . runWriterT . runSocketM

trigger :: EventMap -> Event -> IO ()
trigger eventMap event = case Map.lookup event eventMap of
    Just handlers   -> mapM_ (\h -> h event) handlers
    Nothing         -> return ()