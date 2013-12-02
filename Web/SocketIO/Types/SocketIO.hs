{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Types.SocketIO where

import Web.SocketIO.Types.String

data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

data Configuration = Configuration
    {   transports :: [Transport]
    ,   logLevel :: Int
    ,   heartbeats :: Bool
    ,	closeTimeout :: Int
    ,	heartbeatTimeout :: Int
    ,	heartbeatInterval :: Int
    ,	pollingDuration :: Int
    } deriving Show

type Port = Int
