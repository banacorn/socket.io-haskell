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
--import qualified    Data.ByteString                         as B
--import              Data.IORef                              (readIORef)
--import qualified    Data.HashMap.Strict                     as H

import              Web.SocketIO.Types
import              Web.SocketIO.Connection


data Operation = OpHandshake
               | OpConnect 
               | OpDisconnect 
               | OpEmit (Either (Maybe Int) Event)
               deriving Show
type CompiledOperation = (SessionID, Operation)
data Sitzung = Sitzung SessionID [Operation] deriving Show
data Scheme = Scheme [Event] [CompiledOperation] deriving Show

instance Arbitrary SessionID where
    arbitrary = fromString <$> vectorOf 20 (elements alphaNum)
        where   alphaNum = ['A' .. 'Z']
                        ++ ['a' .. 'z']
                        ++ ['0' .. '9']

instance Arbitrary Event where
    arbitrary = fromString . (:) '#' <$> vectorOf 4 (elements ['a' .. 'z'])

instance Arbitrary Operation where
    arbitrary = do
        nth <- arbitrary
        frequency
            [ (0, elements [OpHandshake, OpConnect, OpDisconnect])
            , (5, elements [OpEmit (Left nth)])
            ]

instance Arbitrary Sitzung where
    arbitrary = do
        oneof [wellformed, illformed]
        where   wellformed = do
                    sessionID <- arbitrary
                    emits <- map (OpEmit . Left) <$> listOf arbitrary
                    return $ Sitzung sessionID ([OpHandshake, OpConnect] ++ emits ++ [OpDisconnect])
                illformed = do
                    sessionID <- arbitrary
                    operations <- arbitrary
                    return $ Sitzung sessionID operations

shuffle :: [Int] -> [[a]] -> [a]
shuffle seeds seqs = reverse $ shuffle' seeds seqs' []
    where   seqs' = filter (not . null) $ seqs

            shuffle' [] _ acc = acc
            shuffle' _ [] acc = acc
            shuffle' (r:rs) seqs_ acc = shuffle' rs seqs_' (picked:acc)
                where   n = length seqs_
                        i = if n == 0 then 0 else r `mod` n
                        (picked, seqs_') = takeHeadAway i seqs_

takeHeadAway :: Int -> [[a]] -> (a, [[a]])
takeHeadAway i sq = (x, sq')
    where   front = take i sq
            (x:xs) = head (drop i sq)
            rear = tail (drop i sq)
            removeNullSeq = filter (not . null)
            sq' = removeNullSeq $ front ++ [xs] ++ rear

compileOperation :: [Event] -> SessionID -> Operation -> CompiledOperation
compileOperation _ i OpHandshake = (i, OpHandshake)
compileOperation _ i OpConnect = (i, OpConnect)
compileOperation _ i OpDisconnect = (i, OpDisconnect)
compileOperation _ i (OpEmit (Right e')) = (i, OpEmit (Right e'))
compileOperation _ i (OpEmit (Left Nothing)) = (i, OpEmit (Right "#some other event"))
compileOperation [] i (OpEmit (Left (Just _))) = (i, OpEmit (Right "#some other event"))
compileOperation e i (OpEmit (Left (Just n))) = (i, OpEmit (Right e'))
    where   index = n `mod` length e
            e' = e !! index

compileSitzung :: [Event] -> Sitzung -> [CompiledOperation]
compileSitzung e (Sitzung i operations) = map (compileOperation e i) operations

instance Arbitrary Scheme where
    arbitrary = do
        events <- arbitrary 
        sitzungen <- listOf arbitrary
        let compiledOperations = map (compileSitzung events) sitzungen
        seeds <- vectorOf (length $ concat compiledOperations) arbitrary
        let shuffledOperations = shuffle seeds compiledOperations
        return $ Scheme events shuffledOperations
s :: IO ()
s = sample (arbitrary :: Gen Sitzung) 


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

--compileScheme :: Scheme -> CompiledScheme
--compileScheme (Scheme events operations) = CompiledScheme events operations'
--    where   operations' = 

--runScheme :: Scheme -> IO Bool
--runScheme (Scheme sessionID events operations) = do

--    env <- makeEnvironment
--    res <- runConnection env Handshake
--    print "good"

--testScheme :: Property
--testScheme = monadicIO $ forAllM arbitrary (run . runScheme)

test :: Framework.Test
test = undefined
--test = testProperty "Schemes" testScheme