{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Server where

import SocketIO.Util
import SocketIO.Type
import SocketIO.Session
import SocketIO.Request
import SocketIO.Event

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp     (run)
import Network.HTTP.Types           (status200)

import Control.Concurrent           (threadDelay)            
import Control.Concurrent.MVar   
import Control.Monad.Trans          (liftIO)
import Control.Monad.Reader       
import Control.Monad.Writer       

banana :: Request -> SessionM Wai.Response
banana Handshake = do
    sessionID <- createSession
    return $ text (sessionID <> ":60:60:xhr-polling")
banana (Connect sessionID) = do
    status <- lookupSession sessionID
    case status of
        Connecting -> do
            updateSession sessionID Connected
            return (text "1::")
        Connected -> do
            liftIO $ threadDelay $ 5 * 1000000
            return (text "8::")
        _ -> do
            return (text "7:::Disconnected")
banana (Disconnect sessionID) = do
    deleteSession sessionID
    return $ text "1"
banana (Emit sessionID trigger) = do
    liftIO $ print trigger
    return $ text "1"
--banana _ = return $ text "1::"

runSession :: Env -> SessionM a -> IO a
runSession env m = runReaderT (runSessionM m) env

text = Wai.responseLBS status200 header . fromText

server :: SocketM () -> IO ()
server handler = do
    table <- newTable
    listeners <- extractListener handler
    print $ map fst listeners
    run 4000 $ \httpRequest -> do
        req <- liftIO $ processRequest httpRequest
        liftIO $ runSession (Env table) (banana req)

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]