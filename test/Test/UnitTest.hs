{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.UnitTest where

import qualified    Test.Framework                          as Framework
import              Test.Framework
import              Test.HUnit
import              Test.Framework.Providers.HUnit
import              Control.Concurrent.Chan
import              Control.Concurrent                      (threadDelay)
--import              Control.Monad                           (forever)
import qualified    Data.ByteString                         as B
import              Data.IORef                              (readIORef)
import qualified    Data.HashMap.Strict                     as H

import              Web.SocketIO.Types
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

    tableRef <- newSessionTable

    let handler = return ()

    stdout <- newChan :: IO (Chan String)

    globalChannel <- newChan :: IO (Buffer)

    --forkIO . forever $ do
    --    readChan stdout >>= putStrLn 

    return $ Env tableRef handler testConfig stdout globalChannel

run :: Env -> Request -> IO ByteString
run env req = runConnection env req

-- The body of the response should contain the session id (sid) given to
-- the client, followed by the heartbeat timeout, the connection closing
-- timeout, and the list of supported transports separated by :
-- 
-- The absence of a heartbeat timeout ('') is interpreted as the server
-- and client not expecting heartbeats.
testWellFormedHandShake :: Assertion
testWellFormedHandShake = do
    env <- makeEnvironment
    res <- run env Handshake
    expectedHandshakeResponse env @=? (B.drop 20 res)

expectedHandshakeResponse :: Env -> ByteString
expectedHandshakeResponse env =  ":" <> heartbeatTimeout'
                              <> ":" <> closeTimeout'
                              <> ":" <> transports'
    where   config = envConfiguration env
            heartbeatTimeout' = if heartbeats config 
                then fromString $ show $ heartbeatTimeout config
                else ""
            closeTimeout' = fromString $ show $ closeTimeout config
            transports' = B.intercalate "," $ map serialize (transports config)

sessionTableSize :: Env -> IO Int
sessionTableSize env = do
    table <- readIORef (envSessionTable env)
    return (H.size table)

sessionLookup :: Env -> SessionID -> IO (Maybe Session)
sessionLookup env sessionID = do
    table <- readIORef (envSessionTable env)
    return (H.lookup sessionID table)

testHandShakeSession :: Assertion
testHandShakeSession = do
    env <- makeEnvironment

    -- before
    size <- sessionTableSize env
    assertEqual "number of sessions before handshake" 0 size
    
    -- after
    res <- run env Handshake
    size' <- sessionTableSize env
    assertEqual "number of sessions after handshake" 1 size'

    let sessionID = B.take 20 res
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

testConnect :: Assertion
testConnect = do
    env <- makeEnvironment

    -- response
    res <- run env Handshake
    let sessionID = B.take 20 res
    res' <- run env (Connect sessionID)
    assertEqual "response with 1::" "1::" res'

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
    res <- run env Handshake
    let sessionID = B.take 20 res
    run env (Connect sessionID)
    res' <- run env (Disconnect sessionID)
    assertEqual "response with 1" "1" res'

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
    [ testGroup "Handshake"
        [ testCase "response" testWellFormedHandShake
        , testCase "session management" testHandShakeSession
        ]
    , testCase "Connect" testConnect
    , testCase "Disconnect" testDisconnect
    ]