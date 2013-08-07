{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Server where

-- web server
import Web.Scotty
import Control.Monad.Trans (liftIO)

import qualified Data.Text.Lazy as TL
import Control.Concurrent (forkIO, threadDelay)
import Data.IORef
import qualified Data.Map as Map
import System.Random

import Data.Monoid (mconcat)

import SocketIO.Message.Type


type Text = TL.Text
type SessionID = TL.Text
type SessionMap = Map.Map SessionID State

server = scotty 4000 $ do

    sessionMapRef <- liftIO (newIORef emptySessionMap)

    get "/socket.io/1/:transport/:session" $ do
        sessionKey <- param "session" :: ActionM Text
        modifyHeader 3000

        response <- modifySession sessionMapRef sessionKey
        text . TL.pack . show $ response

    post "/socket.io/1/:transport/:session" $ do
        text "1"

    get "/socket.io/1" $ do
        modifyHeader 3000
        sessionKey <- issueSession sessionMapRef
        text $ sessionKey `TL.append` ":60:60:xhr-polling"


    where
        emptySessionMap :: SessionMap
        emptySessionMap = Map.empty

        issueSession :: IORef SessionMap -> ActionM Text
        issueSession ref = liftIO $ do
            number <- randomRIO (0, 99999999999999999999) :: IO Integer
            let sessionID = TL.pack $ show number
            modifyIORef ref (Map.insert sessionID Connecting)
            return sessionID

        modifySession :: IORef SessionMap -> SessionID -> ActionM Message
        modifySession ref sessionID = liftIO $ do
            sessionMap <- readIORef ref
            let session = sessionMap Map.! sessionID
            case session of
                Connecting -> do
                    modifyIORef ref (Map.adjust (const Connected) sessionID)
                    return $ Message Connect Omitted (MessageEndpoint "") MessageDataAbsent
                Connected -> do
                    threadDelay (pollingDuration * 1000000)
                    return $ Message Noop Omitted (MessageEndpoint "") MessageDataAbsent

        pollingDuration = 20

modifyHeader port = do
    header "Connection"                         "keep-alive"
    header "Access-Control-Allow-Credentials"   "true"
    header "Access-Control-Allow-Origin"    $   "http://localhost:" `TL.append` TL.pack (show port)
