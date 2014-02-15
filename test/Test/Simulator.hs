{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Simulator where

import qualified    Test.Framework                          as Framework
import              Test.Framework.Providers.QuickCheck2
import              Test.QuickCheck
import              Test.QuickCheck.Monadic

import              Control.Applicative                     ((<$>))
--import              Control.Concurrent                      (threadDelay)
import              Control.Monad.Writer
import              Control.Monad.Reader
--import              Data.IORef                              (readIORef)
--import qualified    Data.HashMap.Strict                     as H
import qualified    Data.ByteString                         as B
import              System.IO.Unsafe                        (unsafePerformIO)

import              Web.SocketIO.Types
import              Web.SocketIO.Connection
import              Web.SocketIO.Channel

data Operation = OpConnect 
               | OpDisconnect 
               | OpEmit Event
               deriving Show
data Scheme = Scheme [EventName] [Operation] deriving Show

--instance Arbitrary SessionID where
--    arbitrary = fromString <$> vectorOf 20 (elements alphaNum)
--        where   alphaNum = ['A' .. 'Z']
--                        ++ ['a' .. 'z']
--                        ++ ['0' .. '9']


instance Arbitrary EventName where
    arbitrary = fromString . (:) '#' <$> vectorOf 4 (elements ['a' .. 'z'])

arbitraryPayload :: Gen Payload
arbitraryPayload = fromString . (:) '>' <$> vectorOf 10 (elements ['a' .. 'z'])

arbitraryEvent :: [EventName] -> Gen Event
arbitraryEvent [] = do
        payloads <- listOf arbitraryPayload
        return (Event "#####" payloads)
arbitraryEvent eventNames = do
        event <- elements eventNames
        payloads <- listOf arbitraryPayload
        return (Event event payloads)

arbitraryOperation :: [EventName] -> Gen Operation
arbitraryOperation eventNames = do
    event <- arbitraryEvent eventNames
    frequency
        [ (1, elements [OpConnect, OpDisconnect])
        , (2, elements [OpEmit event])
        ]

instance Arbitrary Scheme where
    arbitrary = oneof [wellformed]
        where   wellformed = do
                    eventNames <- arbitrary
                    operations <- map OpEmit <$> listOf (arbitraryEvent eventNames)
                    return (Scheme eventNames ([OpConnect] ++ operations ++ [OpDisconnect]))
                --illformed = do
                --    eventNames <- arbitrary
                --    operations <- listOf (arbitraryOperation eventNames)
                --    return (Scheme eventNames operations)

instance Arbitrary (HandlerM a) where
    arbitrary = undefined

e :: IO ()
e = sample (arbitraryOperation ["#aaaa", "#bbbb", "#cccc"])

--s :: IO ()
--s = sample (arbitrary :: Gen (Listener))

scheme :: Scheme
scheme = Scheme ["#rema","#gbdj","#dznb"] [OpEmit (Event "#dznb" [">pfbgqwosid",">hqrcrqtcwh",">sggwvouqay",">hehbufiurw",">gglzbulmil",">iakgpxnjuc",">mirutplyvh",">kqfyugqqjx",">rejabcrkqy",">gidxxtmlag",">jzbdzgmbsd",">bxnemodtfc",">zmoyuyvhee"])]




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
    ,   pollingDuration = 1
    }

makeEnvironment :: IO Env
makeEnvironment = do 

    tableRef <- newSessionTableRef

    let handler = return ()

    logChhannel <- newLogChannel
    globalChannel <- newGlobalChannel

    --streamToStdout logChhannel

    return $ Env tableRef handler testConfig logChhannel globalChannel


runScheme :: Scheme -> IO Bool
runScheme (Scheme _ operations) = do

    env <- makeEnvironment

    MsgRaw raw <- runConnection env Handshake
    let sessionID = B.take 20 raw

    res <- mapM (runConnection env . translate sessionID) operations
    print res

    return True

a :: Scheme
a = Scheme [] [OpConnect, OpConnect]

testScheme :: Property
testScheme = monadicIO $ do
    run $ runScheme a
--testScheme = monadicIO $ forAllM arbitrary (run . runScheme)

test :: Framework.Test
test = testProperty "Schemes" testScheme
--test = undefined

go :: IO Bool
go = runScheme a