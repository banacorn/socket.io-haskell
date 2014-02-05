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
               | OpEmit (Either (Maybe Int) Event) [Payload]
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

--instance Arbitrary Payload where
--    arbitrary = fromString . (:) '>' <$> vectorOf 10 (elements ['a' .. 'z'])


instance Arbitrary Operation where
    arbitrary = do
        nth <- arbitrary
        payload <- arbitrary
        frequency
            [ (0, elements [OpHandshake, OpConnect, OpDisconnect])
            , (5, elements [OpEmit (Left nth) payload])
            ]

instance Arbitrary Sitzung where
    arbitrary = do
        oneof [wellformed, illformed]
        where   wellformed = do
                    sessionID <- arbitrary
                    payload <- arbitrary
                    emits <- map (\ event -> OpEmit (Left event) payload) <$> listOf arbitrary
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
compileOperation _ i (OpEmit (Right e') p) = (i, OpEmit (Right e') p)
compileOperation _ i (OpEmit (Left Nothing) p) = (i, OpEmit (Right "#some other event") p)
compileOperation [] i (OpEmit (Left (Just _)) p) = (i, OpEmit (Right "#some other event") p)
compileOperation e i (OpEmit (Left (Just n)) p) = (i, OpEmit (Right e') p)
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
s = sample (arbitrary :: Gen Scheme) 

a :: Scheme
a = Scheme ["#enar","#zfuy"] [("IPCd0BS7Zd8j2OBS1RwG",OpHandshake),("IPCd0BS7Zd8j2OBS1RwG",OpConnect),("IPCd0BS7Zd8j2OBS1RwG",OpEmit (Right "#enar") ["#bzsb"]),("IPCd0BS7Zd8j2OBS1RwG",OpEmit (Right "#enar") ["#bzsb"]),("IPCd0BS7Zd8j2OBS1RwG",OpDisconnect)]


b :: Scheme
b = Scheme ["#ozeb","#aafv","#vlfu","#muev","#gdya","#fulf","#wajg"] [("PVZO28fptRycouWNSI2A",OpEmit (Right "#some other event") ["#xwlj","#yknl","#mlvp","#nksg","#mokf","#jqpb","#roml","#qbwd","#cvhm"]),("PVZO28fptRycouWNSI2A",OpEmit (Right "#ozeb") ["#zafb"]),("PVZO28fptRycouWNSI2A",OpEmit (Right "#some other event") ["#luwa","#wvec","#xkmw","#ebga","#nvej","#hant","#vsey"]),("PVZO28fptRycouWNSI2A",OpEmit (Right "#aafv") ["#xlwr","#nvcs","#wbcq"])]

--compiled :: CompiledOperation
--compiled 

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

    res <- mapM (runConnection env . translate) operations
    print res

    return True

    where   translate :: CompiledOperation -> Request
            translate (_, OpHandshake)  = Handshake
            translate (i, OpConnect)    = Connect i
            translate (i, OpDisconnect) = Disconnect i
            translate (i, OpEmit (Left _) _) = Emit i NoEmitter
            translate (i, OpEmit (Right e) p) = Emit i (Emitter e p)

--testScheme :: Property
--testScheme = monadicIO $ forAllM arbitrary (run . runScheme)

test :: Framework.Test
test = undefined
--test = testProperty "Schemes" testScheme