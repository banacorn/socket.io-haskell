{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unit.Handshake where

import qualified    Test.Framework                          as Framework
import              Test.Framework
import              Test.HUnit
import              Test.Framework.Providers.HUnit
import              Control.Concurrent                      (threadDelay)
import              Data.IORef                              (readIORef)
import qualified    Data.HashMap.Strict                     as H

import              Web.SocketIO.Types
import              Web.SocketIO.Channel
import              Web.SocketIO.Connection

testConfig :: Configuration
testConfig = Configuration
    {   transports = [XHRPolling]
    ,   logLevel = 3
    ,   heartbeats = True
    ,   closeTimeout = 2
    ,   heartbeatTimeout = 60
    ,   heartbeatInterval = 25
    ,   pollingDuration = 20
    }

makeEnvironment :: IO Env
makeEnvironment = do 

    tableRef <- newSessionTableRef

    let handler = return ()

    logChhannel <- newLogChannel
    globalChannel <- newGlobalChannel

    return $ Env tableRef handler testConfig logChhannel globalChannel

-- The body of the response should contain the session id (sid) given to
-- the client, followed by the heartbeat timeout, the connection closing
-- timeout, and the list of supported transports separated by :
-- 
-- The absence of a heartbeat timeout ('') is interpreted as the server
-- and client not expecting heartbeats.
testResponse :: Assertion
testResponse = do
    env <- makeEnvironment
    MsgHandshake _ a b t <- runConnection env Handshake
    expectedHandshakeResponse env @=?    (MsgHandshake "" a b t)

expectedHandshakeResponse :: Env -> Message
expectedHandshakeResponse env = MsgHandshake "" heartbeatTimeout' closeTimeout' transports'
    where   config = envConfiguration env
            heartbeatTimeout' = if heartbeats config 
                then heartbeatTimeout config
                else 0
            closeTimeout' = closeTimeout config
            transports' = transports config

sessionTableSize :: Env -> IO Int
sessionTableSize env = do
    table <- readIORef (envSessionTableRef env)
    return (H.size table)

sessionLookup :: Env -> SessionID -> IO (Maybe Session)
sessionLookup env sessionID = do
    table <- readIORef (envSessionTableRef env)
    return (H.lookup sessionID table)

testHandShakeSession :: Assertion
testHandShakeSession = do
    env <- makeEnvironment

    -- before
    size <- sessionTableSize env
    assertEqual "number of sessions before handshake" 0 size
    
    -- after
    MsgHandshake sessionID _ _ _ <- runConnection env Handshake
    size' <- sessionTableSize env
    assertEqual "number of sessions after handshake" 1 size'
    session <- sessionLookup env sessionID
    case session of
        Just (Session i s _ _ _) -> do
            assertEqual "matching session ID" sessionID i
            assertEqual "session status" Connecting s
        _ -> assertFailure "session not found"

    -- timeout
    let closeTimeout' = closeTimeout (envConfiguration env)
    threadDelay (closeTimeout' * 1000000 + 1000000)
    size'' <- sessionTableSize env
    assertEqual "number of sessions after handshake closing timeout" 0 size''

--testConnectSession

test :: Framework.Test
test = testGroup "Handshake" 
    [ testCase "response" testResponse
    --, testCase "session management" testHandShakeSession
    ]