{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Simulator where

import qualified    Test.Framework                  as Framework
import              Test.Framework
import              Test.HUnit
--import Test.HUnit
import              Test.Framework.Providers.HUnit
import              Control.Applicative             ((<$>))
import              Control.Concurrent.Chan
import qualified    Data.ByteString as B

import              Web.SocketIO.Server
import              Web.SocketIO.Types
import              Web.SocketIO.Connection

testConfig :: Configuration
testConfig = Configuration
    {   transports = [XHRPolling, WebSocket]
    ,   logLevel = 3
    ,   heartbeats = False
    ,   closeTimeout = 60
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

--alphaNum :: Gen Char
--alphaNum = elements $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

--instance Arbitrary SessionID where
--    arbitrary = fromString <$> (vectorOf 20 alphaNum)

--instance Arbitrary Text where
--    arbitrary = fromString <$> arbitrary

--instance Arbitrary Emitter where
--    arbitrary = do
--        event <- fromString <$> listOf1 alphaNum
--        payloads <- listOf (fromString <$> listOf1 alphaNum)
--        elements [NoEmitter, Emitter event payloads]

--instance Arbitrary Request where
--    arbitrary = do
--        sessionID <- arbitrary
--        emitter <- arbitrary
--        elements
--            [ Handshake
--            , Disconnect sessionID
--            , Connect sessionID
--            , Emit sessionID emitter
--            ]


-- The body of the response should contain the session id (sid) given to
-- the client, followed by the heartbeat timeout, the connection closing
-- timeout, and the list of supported transports separated by :
-- 
-- The absence of a heartbeat timeout ('') is interpreted as the server
-- and client not expecting heartbeats.
testHandShake :: Assertion
testHandShake = do
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

test :: Framework.Test
test = testGroup "Unit Test"
    [ testCase "Handshake" testHandShake
    ]