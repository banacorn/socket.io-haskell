{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SocketIO.Server where

import SocketIO.Util
import SocketIO.Type

import Network.Wai
import Network.Wai.Handler.Warp     (run)
import Network.HTTP.Types           (status200)

import System.Random                (randomRIO)

import Control.Concurrent           (threadDelay)            
import Control.Monad.Trans          (liftIO)
import Control.Monad.Reader       

import qualified Data.HashTable.IO as H
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.IORef
import Data.List                    (intersperse)


processRequest :: Request -> SocketRequest
processRequest request = case path of
    [n, p]          -> SocketRequest method n  p  "" ""
    [n, p, t]       -> SocketRequest method n  p  t  ""
    [n, p, t, s]    -> SocketRequest method n  p  t  s
    _               -> SocketRequest method "" "" "" ""
    where   method  = requestMethod request
            path    = map TL.fromStrict . cleanup . pathInfo $ request
            cleanup = filter (/= "")

processSocketRequest :: SocketRequest -> Connection
processSocketRequest (SocketRequest _ "" "" "" "") = Disconnection  
processSocketRequest (SocketRequest "GET" n p "" "") = Handshake  
processSocketRequest (SocketRequest "GET" n p t s) = Connection s
processSocketRequest (SocketRequest _ _ _ _ _) = Disconnection  

preprocess = processSocketRequest . processRequest


readTable :: (Table -> IO a) -> SessionM a
readTable f = ask >>= liftIO . readIORef . getSessionTable >>= liftIO . f

createSession :: SessionM SessionID
createSession = readTable $ \table -> do
    sessionID <- genSessionID
    H.insert table sessionID Connecting
    return sessionID
    where   genSessionID = fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: IO Text

updateSession :: SessionID -> Status -> SessionM ()
updateSession sessionID status = readTable $ \table -> do
    H.delete table sessionID
    H.insert table sessionID status

deleteSession :: SessionID -> SessionM ()
deleteSession sessionID = readTable $ \table -> 
    H.delete table sessionID

lookupSession :: SessionID -> SessionM Status
lookupSession sessionID = readTable $ \table -> do
    result <- H.lookup table sessionID
    case result of
        Just status -> return status
        Nothing     -> return Disconnected


server ref req = liftIO . runSession ref $ case req of

    Handshake -> do
        sessionID <- createSession
        return $ text (sessionID <> ":60:60:xhr-polling")

    Connection sessionID -> do
        status <- lookupSession sessionID
        case status of
            Connecting -> do
                updateSession sessionID Connected
                return (text "1::")
            Connected -> do
                return (text "8::")
            _ -> do
                return (text "7:::Disconnected")

    otherwise -> return $ text "1::"



--server Connection = return $ text "1:::hi"
--server _ = return $ text "1::" 


runSession :: Env -> SessionM a -> IO a
runSession s m = liftIO $ runReaderT (runSessionM m) s

newTable :: IO (IORef Table)
newTable = H.new >>= newIORef 

text = responseLBS status200 header . fromText

main = do
    table <- newTable
    run 4000 $ server (Env table) . preprocess

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]