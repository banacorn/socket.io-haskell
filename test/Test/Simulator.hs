{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Simulator where

import qualified    Test.Framework                          as Framework
--import              Test.Framework
--import              Test.Framework.Providers.QuickCheck2
import              Test.QuickCheck
--import              Test.QuickCheck.Monadic

import              Control.Applicative                     ((<$>))
import              Control.Concurrent.Chan                 (Chan, newChan)
--import              Control.Concurrent                      (threadDelay)
----import              Control.Monad                           (forever)
import qualified    Data.ByteString                         as B
--import              Data.IORef                              (readIORef)
--import qualified    Data.HashMap.Strict                     as H

import              Web.SocketIO.Types
import              Web.SocketIO.Connection

data Operation = OpConnect 
               | OpDisconnect 
               | OpEmit Emitter
               deriving Show
data Scheme = Scheme [Event] [Operation] deriving Show

--instance Arbitrary SessionID where
--    arbitrary = fromString <$> vectorOf 20 (elements alphaNum)
--        where   alphaNum = ['A' .. 'Z']
--                        ++ ['a' .. 'z']
--                        ++ ['0' .. '9']


instance Arbitrary Event where
    arbitrary = fromString . (:) '#' <$> vectorOf 4 (elements ['a' .. 'z'])

arbitraryPayload :: Gen Payload
arbitraryPayload = fromString . (:) '>' <$> vectorOf 10 (elements ['a' .. 'z'])

arbitraryEmitter :: [Event] -> Gen Emitter
arbitraryEmitter [] = do
        payloads <- listOf arbitraryPayload
        return (Emitter "#####" payloads)
arbitraryEmitter events = do
        event <- elements events
        payloads <- listOf arbitraryPayload
        return (Emitter event payloads)

arbitraryOperation :: [Event] -> Gen Operation
arbitraryOperation events = do
    emitter <- arbitraryEmitter events
    frequency
        [ (1, elements [OpConnect, OpDisconnect])
        , (2, elements [OpEmit emitter])
        ]

instance Arbitrary Scheme where
    arbitrary = oneof [illformed, wellformed]
        where   illformed = do
                    events <- arbitrary
                    operations <- listOf (arbitraryOperation events)
                    return (Scheme events operations)
                wellformed = do
                    events <- arbitrary
                    operations <- map OpEmit <$> listOf (arbitraryEmitter events)
                    return (Scheme events ([OpConnect] ++ operations ++ [OpDisconnect]))
e :: IO ()
e = sample (arbitraryOperation ["#aaaa", "#bbbb", "#cccc"])

s :: IO ()
s = sample (arbitrary :: Gen Scheme) 

scheme :: Scheme
scheme = Scheme ["#rema","#gbdj","#dznb"] [OpEmit (Emitter "#dznb" [">pfbgqwosid",">hqrcrqtcwh",">sggwvouqay",">hehbufiurw",">gglzbulmil",">iakgpxnjuc",">mirutplyvh",">kqfyugqqjx",">rejabcrkqy",">gidxxtmlag",">jzbdzgmbsd",">bxnemodtfc",">zmoyuyvhee"])]




translate :: SessionID -> Operation -> Request
translate i OpConnect = Connect i
translate i OpDisconnect = Disconnect i
translate i (OpEmit emitter) = Emit i emitter

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


runScheme :: Scheme -> IO Bool
runScheme (Scheme events operations) = do

    env <- makeEnvironment

    sessionID <- B.take 20 <$> runConnection env Handshake

    res <- mapM (runConnection env . translate sessionID) operations
    print res

    return True



--testScheme :: Property
--testScheme = monadicIO $ forAllM arbitrary (run . runScheme)

test :: Framework.Test
test = undefined
--test = testProperty "Schemes" testScheme