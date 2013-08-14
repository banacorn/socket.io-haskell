{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SocketIO.Server where

import SocketIO.Util

import Network.Wai
import Network.Wai.Handler.Warp     (run)
import Network.HTTP.Types           (status200, Method)

import System.Random                (randomRIO)

import Control.Concurrent (threadDelay)            
import Control.Monad.ST            
import Control.Monad.Trans          (liftIO)
import Control.Monad.Reader       

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Monoid                  (mconcat)
import Data.List                    (intersperse)
import Data.IORef
import qualified Data.HashTable.IO as H
type HashTable k v = H.LinearHashTable k v

type Text = TL.Text
type Namespace = Text
type Protocol = Text
type Transport = Text
type SessionID = Text 
data Status = Connecting | Connected | Disconnecting | Disconnected deriving Show
type Session = (SessionID, Status)
type Table = HashTable SessionID Status 
data SocketRequest = SocketRequest Method Namespace Protocol Transport SessionID deriving (Show)

data Connection = Handshake | Connection SessionID | Disconnection deriving Show 

newtype SessionRefM b a = SessionRefM { runSessionM :: (ReaderT (IORef b) IO) a }
    deriving (Monad, Functor, MonadIO, MonadReader (IORef b))

type SessionM a = SessionRefM Table a

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


readTable :: SessionM Table
readTable = ask >>= liftIO . readIORef

createSession :: SessionM SessionID
createSession = do
    table <- readTable
    sessionID <- liftIO genSessionID
    liftIO $ H.insert table sessionID Connecting
    liftIO $ H.toList table >>=  print
    liftIO $ print "======="

    return sessionID
    where   genSessionID = fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: IO Text

updateSession :: SessionID -> Status -> SessionM ()
updateSession sessionID status = do
    table <- readTable
    liftIO $ do
        H.delete table sessionID
        H.insert table sessionID status

deleteSession :: SessionID -> SessionM ()
deleteSession sessionID = do
    table <- readTable
    liftIO $ H.delete table sessionID

lookupSession :: SessionID -> SessionM Status
lookupSession sessionID = do
    table <- readTable
    liftIO $ H.toList table >>=  print
    result <- liftIO $ H.lookup table sessionID
    liftIO $ case result of
        Just status -> return status
        Nothing -> return Disconnected

haha :: SessionM ()
haha = do
    t <- readTable
    liftIO $ (H.toList t) >>= print
    return ()

foo = do
    t <- newTable
    runReaderT (runSessionM haha) t

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


runSession :: (IORef Table) -> SessionM a -> IO a
runSession s m = liftIO $ runReaderT (runSessionM m) s

newTable :: IO (IORef Table)
newTable = H.new >>= newIORef 

text = responseLBS status200 header . fromText

main = do
    table <- newTable
    run 4000 $ server table . preprocess

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]