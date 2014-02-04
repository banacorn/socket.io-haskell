{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Simulator where

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

testHandShakeSession :: Assertion
testHandShakeSession = do
    env <- makeEnvironment

    -- before
    table <- readIORef (envSessionTable env)
    assertEqual "number of sessions before handshake" 0 (H.size table)
    
    -- after
    run env Handshake
    table' <- readIORef (envSessionTable env)
    assertEqual "number of sessions after handshake" 1 (H.size table')

    -- timeout
    let closeTimeout' = closeTimeout (envConfiguration env)
    threadDelay (closeTimeout' * 1000000 + 1000000)
    table'' <- readIORef (envSessionTable env)
    assertEqual "number of sessions after handshake closing timeout" 0 (H.size table'')


test :: Framework.Test
test = testGroup "Handshake"
    [ testCase "response" testWellFormedHandShake
    , testCase "session management" testHandShakeSession
    ]