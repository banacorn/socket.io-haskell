{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unit where

import qualified    Test.Unit.Handshake                     as Handshake

import qualified    Test.Framework                          as Framework
import              Test.Framework
import              Test.HUnit
import              Test.Framework.Providers.HUnit
import              Control.Concurrent                      (threadDelay)
--import              Control.Monad                           (forever)
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

run :: Env -> Request -> IO Message
run env req = runConnection env req

sessionTableSize :: Env -> IO Int
sessionTableSize env = do
    table <- readIORef (envSessionTableRef env)
    return (H.size table)

sessionLookup :: Env -> SessionID -> IO (Maybe Session)
sessionLookup env sessionID = do
    table <- readIORef (envSessionTableRef env)
    return (H.lookup sessionID table)

testConnect :: Assertion
testConnect = do
    env <- makeEnvironment

    -- response
    MsgHandshake sessionID _ _ _ <- run env Handshake
    res' <- run env (Connect sessionID)
    assertEqual "response with 1::" "1::" (serialize res' :: ByteString)

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

    
testDisconnect :: Assertion
testDisconnect = do
    env <- makeEnvironment

    -- response
    MsgHandshake sessionID _ _ _ <- run env Handshake
    run env (Connect sessionID)
    res' <- run env (Disconnect sessionID)
    assertEqual "response with Noop" "8:::" (serialize res' :: ByteString)

    -- session gone
    session <- sessionLookup env sessionID
    case session of
        Just (Session _ _ _ _ _) -> assertFailure "session not found"
        _ -> return ()

    size <- sessionTableSize env
    assertEqual "number of sessions after disconnected" 0 size



--testConnectSession

test :: Framework.Test
test = testGroup "Unit Tests" 
    [ Handshake.test
    , testCase "Connect" testConnect
    , testCase "Disconnect" testDisconnect
    ]