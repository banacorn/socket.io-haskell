{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Server (server) where

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
import Control.Applicative          ((<$>), (<*>))
import Control.Monad.Trans          (liftIO)
import Control.Monad.Reader       
import Control.Monad.Writer       

debug :: String -> SessionM ()
debug = liftIO . print

--banana :: Request -> SessionM Wai.Response
--banana Handshake = do
--    sessionID <- createSession
--    debug $ "[Handshake]    " ++ fromText sessionID
--    return $ text (sessionID <> ":60:60:xhr-polling")
--banana (Connect sessionID) = do
--    result <- lookupSession sessionID
--    case result of
--        Session status _ -> do
--            case status of
--                Connecting -> do
--                    debug $ "[Connecting]  " ++ fromText sessionID
--                    updateSession sessionID updateStatus
--                    return (text "1::")
--                Connected -> do
--                    result <- timeout (pollingDuration * 1000000) (flushBuffer sessionID)
--                    case result of
--                        Just r  -> do
--                            debug $ "[Polling]*    " ++ fromText sessionID
--                            return (text $ toMessage (MsgEvent NoID NoEndpoint r))
--                        Nothing -> do
--                            debug $ "[Polling]     " ++ fromText sessionID
--                            return (text "8::")
--                _ -> do
--                    return (text "7:::Disconnected")
--        NoSession -> return (text "7:::Disconnected")
--    where   updateStatus (Session _ b) = return $ Session Connected b
--banana (Disconnect sessionID) = do
--    deleteSession sessionID
--    return $ text "1"
--banana (Emit sessionID emitter) = do
--    debug $ "[Emit]         " ++ fromText sessionID
--    session <- lookupSession sessionID
--    case session of
--        Session status channel  -> do
--            triggerListener emitter channel
--        NoSession               -> do
--            return ()
--    return $ text "1"


handleConnection RHandshake = createSession $ do
    sessionID <- getSessionID <$> ask
    debug $ "[Handshake]    " ++ fromText sessionID
    text $ sessionID <> ":60:60:xhr-polling"

--handleConnection (RConnection sessionID) = 

runConnection :: Env -> ConnectionM a -> IO a
runConnection env m = runReaderT (runConnectionM m) env

text = return . Wai.responseLBS status200 header . fromText

server :: SocketM () -> IO ()
server handler = do
    env <- initialEnv handler
    run 4000 $ \httpRequest -> liftIO $ do
        req <- processRequest httpRequest
        runConnection env (handleConnection req)

initialEnv :: SocketM () -> IO Env
initialEnv handler = do
    table <- newTableRef
    return (Env table handler)



pollingDuration = 20

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]