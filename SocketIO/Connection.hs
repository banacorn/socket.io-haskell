module SocketIO.Connection (runConnection, newSessionTable)  where

import SocketIO.Type
import SocketIO.Util
import SocketIO.Session

import Data.IORef.Lifted
import qualified Data.HashTable.IO as H
import System.Random (randomRIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader       
import Control.Monad.Writer       
import Control.Concurrent.Chan.Lifted
import Control.Applicative          ((<$>), (<*>))

newSessionTable :: IO (IORef Table)
newSessionTable = H.new >>= newIORef

executeHandler :: SocketM () -> Buffer -> ConnectionM [Listener]
executeHandler handler buffer = liftIO $ execWriterT (runReaderT (runSocketM handler) buffer)

runConnection :: Env -> Request -> IO Text
runConnection env req = do
    runReaderT (runConnectionM (handleConnection req)) env


handleConnection :: Request -> ConnectionM Text
handleConnection RHandshake = do
    buffer <- newChan  
    handler <- getHandler <$> ask
    sessionID <- genSessionID
    listeners <- executeHandler handler buffer

    let session = Session sessionID Connecting buffer listeners

    table <- getTable
    liftIO $ H.insert table sessionID session
    liftIO $ H.toList table >>= print . show . map fst
    runSession Syn session
    where   genSessionID = liftIO $ fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: ConnectionM Text

handleConnection (RConnect sessionID) = do
    table <- getTable
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just (Session sessionID status buffer listeners) -> do
            let session = Session sessionID Connected buffer listeners
            case status of
                Connecting -> do
                    liftIO $ H.delete table sessionID
                    liftIO $ H.insert table sessionID session
                    runSession Ack session
                Connected ->
                    runSession Polling session
        Nothing -> do
            debug $ "[Error]      Unable to find session " ++ fromText sessionID
            runSession Error NoSession

handleConnection (RDisconnect sessionID) = do
    table <- getTable
    liftIO $ H.delete table sessionID
    runSession Disconnect NoSession

handleConnection (REmit sessionID emitter) = do
    table <- getTable
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just session -> runSession (Emit emitter) session
        Nothing      -> runSession Error NoSession


getTable :: ConnectionM Table
getTable = ask >>= readIORef . getSessionTable
