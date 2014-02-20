{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unit.Session where

--------------------------------------------------------------------------------
import qualified    Test.Framework                          as Framework
import              Test.Framework
import              Test.HUnit
import              Test.Framework.Providers.HUnit
import              Control.Concurrent                      (threadDelay)
import              Data.IORef                              (readIORef)
import qualified    Data.HashMap.Strict                     as H
--------------------------------------------------------------------------------

import              Web.SocketIO.Types
import              Web.SocketIO.Channel
import              Web.SocketIO.Connection
--------------------------------------------------------------------------------
testConfig :: Configuration
testConfig = Configuration
    {   transports = [XHRPolling]
    ,   logLevel = 3
    ,   logTo = stderr
    ,   heartbeats = True
    ,   closeTimeout = 1
    ,   heartbeatTimeout = 60
    ,   heartbeatInterval = 25
    ,   pollingDuration = 20
    }
--------------------------------------------------------------------------------
makeEnvironment :: IO Env
makeEnvironment = do 

    tableRef <- newSessionTableRef

    let handler = return ()

    logChhannel <- newLogChannel
    globalChannel <- newGlobalChannel

    return $ Env tableRef handler testConfig logChhannel globalChannel

--------------------------------------------------------------------------------
sessionTableSize :: Env -> IO Int
sessionTableSize env = do
    table <- readIORef (envSessionTableRef env)
    return (H.size table)

sessionLookup :: Env -> SessionID -> IO (Maybe Session)
sessionLookup env sessionID = do
    table <- readIORef (envSessionTableRef env)
    return (H.lookup sessionID table)
--------------------------------------------------------------------------------
testHandshake :: Assertion
testHandshake = do
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
--------------------------------------------------------------------------------
testConnect :: Assertion
testConnect = do
    env <- makeEnvironment
    MsgHandshake sessionID _ _ _ <- runConnection env Handshake
    runConnection env (Connect sessionID)

    -- session status
    session <- sessionLookup env sessionID
    case session of
        Just (Session i s _ _ _) -> do
            assertEqual "matching session ID" sessionID i
            assertEqual "session status" Connected s
        _ -> assertFailure "session not found"

    -- timeout
    let closeTimeout' = closeTimeout (envConfiguration env)
    threadDelay (closeTimeout' * 1000000 + 1000000)

    size <- sessionTableSize env
    assertEqual "number of sessions after connect closing timeout" 0 size

--------------------------------------------------------------------------------
testDisconnect :: Assertion
testDisconnect = do
    env <- makeEnvironment

    -- response
    MsgHandshake sessionID _ _ _ <- runConnection env Handshake
    runConnection env (Connect sessionID)
    runConnection env (Disconnect sessionID)

    -- session gone
    session <- sessionLookup env sessionID
    case session of
        Just (Session _ _ _ _ _) -> assertFailure "session not found"
        _ -> return ()

    size <- sessionTableSize env
    assertEqual "number of sessions after client force disconnected" 0 size
--------------------------------------------------------------------------------
test :: Framework.Test
test = testGroup "Session" 
    [ testCase "Handshake" testHandshake
    , testCase "Connect" testConnect
    , testCase "Disconnect" testDisconnect
    ]