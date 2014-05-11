{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Simulator where

import              Test.Instances

import qualified    Test.Framework                          as Framework
import              Test.Framework.Providers.QuickCheck2
import              Test.QuickCheck
import              Test.QuickCheck.Monadic

--import              Control.Applicative                     ((<$>))
--import              Control.Concurrent                      (threadDelay)
--import              Control.Monad.Writer
--import              Control.Monad.Reader
--import              Data.IORef                              (readIORef)
--import qualified    Data.HashMap.Strict                     as H

import              Web.SocketIO.Types
import              Web.SocketIO.Connection
import              Web.SocketIO.Channel

--data State = State
--    {   sessionTable :: SessionTable
--    }

--scheme :: Scheme
--scheme = Scheme ["#rema","#gbdj","#dznb"] [OpEmit (Event "#dznb" [">pfbgqwosid",">hqrcrqtcwh",">sggwvouqay"])]

--translate :: SessionID -> Operation -> Request
--translate i OpConnect = Connect i
--translate i OpDisconnect = Disconnect i
--translate i (OpEmit emitter) = Emit i emitter

--testConfig :: Configuration
--testConfig = Configuration
--    {   transports = [XHRPolling]
--    ,   logLevel = 3
--    ,   logTo = stderr
--    ,   heartbeats = True
--    ,   closeTimeout = 2
--    ,   heartbeatTimeout = 60
--    ,   heartbeatInterval = 25
--    ,   pollingDuration = 1
--    }

--makeEnvironment :: IO Env
--makeEnvironment = do 

--    tableRef <- newSessionTableRef

--    let handler = return ()

--    logChhannel <- newLogChannel
--    globalChannel <- newGlobalChannel

--    --streamToStdout logChhannel

--    return $ Env tableRef handler testConfig logChhannel globalChannel


--runScheme :: Scheme -> IO Bool
--runScheme (Scheme _ operations) = do

--    env <- makeEnvironment

--    MsgHandshake sessionID _ _ _ <- runConnection env Handshake

--    res <- mapM (runConnection env . translate sessionID) operations
--    print res

--    return True

--a :: Scheme
--a = Scheme [] [OpConnect, OpConnect]

--testScheme :: Property
--testScheme = monadicIO $ do
--    run $ runScheme a
----testScheme = monadicIO $ forAllM arbitrary (run . runScheme)

--test :: Framework.Test
--test = testProperty "Schemes" testScheme
----test = undefined

--go :: IO Bool
--go = runScheme a