{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unit.Connection where

import qualified    Test.Framework                          as Framework
import              Test.Framework
import              Test.HUnit
import              Test.Framework.Providers.HUnit

import              Web.SocketIO.Types
import              Web.SocketIO.Channel
import              Web.SocketIO.Connection
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
testHandshake :: Assertion
testHandshake = do
    env <- makeEnvironment
    MsgHandshake _ a b t <- runConnection env Handshake
    expectedResponse env @=? (MsgHandshake "" a b t)
    where   expectedResponse env = MsgHandshake "" heartbeatTimeout' closeTimeout' transports'
                where   config = envConfiguration env
                        heartbeatTimeout' = if heartbeats config 
                            then heartbeatTimeout config
                            else 0
                        closeTimeout' = closeTimeout config
                        transports' = transports config
--------------------------------------------------------------------------------
testConnect :: Assertion
testConnect = do
    env <- makeEnvironment
    MsgHandshake sessionID _ _ _ <- runConnection env Handshake
    res' <- runConnection env (Connect sessionID)
    assertEqual "response with 1::" "1::" (serialize res' :: ByteString)
    
testDisconnect :: Assertion
testDisconnect = do
    env <- makeEnvironment
    MsgHandshake sessionID _ _ _ <- runConnection env Handshake
    runConnection env (Connect sessionID)
    res' <- runConnection env (Disconnect sessionID)
    assertEqual "response with Noop" "8:::" (serialize res' :: ByteString)

--------------------------------------------------------------------------------
test :: Framework.Test
test = testGroup "Connection" 
    [ testCase "Handshake"  testHandshake
    , testCase "Connect"    testConnect
    , testCase "Disconnect" testDisconnect
    ]