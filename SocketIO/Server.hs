{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Server where

import SocketIO.Util
import SocketIO.Type
import SocketIO.Session
import SocketIO.Request
import SocketIO.Event

import System.Timeout.Lifted

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp     (run)
import Network.HTTP.Types           (status200)

import Control.Concurrent.Lifted    (threadDelay)            
import Control.Monad.Trans          (liftIO)
import Control.Monad.Reader       
import Control.Monad.Writer       

debug :: String -> SessionM ()
debug = liftIO . print

banana :: Request -> SessionM Wai.Response
banana Handshake = do
    (sessionID, Session _ channel) <- createSession
    debug $ "[Handshake]    " ++ fromText sessionID
    handler <- fmap getHandler ask
    listeners <- executeHandler handler channel
    registerListener listeners

    return $ text (sessionID <> ":60:60:xhr-polling")
banana (Connect sessionID) = do
    Session status _ <- lookupSession sessionID
    case status of
        Connecting -> do
            debug $ "[Connecting]  " ++ fromText sessionID
            updateSession sessionID updateStatus
            return (text "1::")
        Connected -> do
            result <- timeout (10 * 1000000) (flushBuffer sessionID)
            case result of
                Just r  -> do
                    debug $ "[Polling]*    " ++ fromText sessionID
                    return (text $ toMessage (MsgEvent NoID NoEndpoint r))
                Nothing -> do
                    debug $ "[Polling]     " ++ fromText sessionID
                    return (text "8::")
        _ -> do
            return (text "7:::Disconnected")
    where   updateStatus (Session _ b) = return $ Session Connected b
banana (Disconnect sessionID) = do
    deleteSession sessionID
    return $ text "1"
banana (Emit sessionID emitter) = do
    debug $ "[Emit]         " ++ fromText sessionID
    session <- lookupSession sessionID
    case session of
        Session status channel  -> do
            triggerListener emitter channel
        NoSession               -> do
            return ()
    return $ text "1"

runSession :: Env -> SessionM a -> IO a
runSession env m = runReaderT (runSessionM m) env

text = Wai.responseLBS status200 header . fromText

server :: SocketM () -> IO ()
server handler = do
    env <- initialEnv handler
    run 4000 $ \httpRequest -> do
        req <- liftIO $ processRequest httpRequest
        liftIO $ runSession env (banana req)

--initialEnv :: SocketM () -> 
initialEnv handler = do
    table <- newTable
    listeners <- newListenerList
    return (Env table handler listeners)

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]