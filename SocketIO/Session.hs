{-# LANGUAGE OverloadedStrings #-}
module SocketIO.Session (
    createSession, 
    updateSession, 
    deleteSession, 
    lookupSession,
    writeBuffer,
    flushBuffer,
    newTable
) where

import SocketIO.Type
import SocketIO.Util

import System.Random (randomRIO)
import Control.Monad.Reader       
import Control.Concurrent.Chan.Lifted
import Data.IORef.Lifted
import qualified Data.HashTable.IO as H

newTable :: IO (IORef Table)
newTable = H.new >>= newIORef

readTable :: (Table -> SessionM a) -> SessionM a
readTable f = ask >>= readIORef . getSessionTable >>= f

createSession :: SessionM (SessionID, Session)
createSession = readTable $ \table -> do
    sessionID <- genSessionID
    chan <- newChan
    liftIO $ H.insert table sessionID $ Session Connecting chan
    return (sessionID, Session Connecting chan)
    where   genSessionID = liftIO $ fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: SessionM Text

updateSession :: SessionID -> (Session -> SessionM Session) -> SessionM ()
updateSession sessionID f = readTable $ \table -> do
    result <- liftIO $ H.lookup table sessionID
    liftIO $ H.delete table sessionID
    case result of
        Just session -> liftIO . H.insert table sessionID =<< f session
        Nothing      -> return ()

deleteSession :: SessionID -> SessionM ()
deleteSession sessionID = readTable $ \table -> 
    liftIO $ H.delete table sessionID

lookupSession :: SessionID -> SessionM Session
lookupSession sessionID = readTable $ \table -> do
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just session -> return session
        Nothing      -> return NoSession

writeBuffer :: SessionID -> Emitter -> SessionM ()
writeBuffer sessionID emitter = readTable $ \table -> do
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just (Session _ chan)  -> writeChan chan emitter
        Nothing                     -> return ()

flushBuffer :: SessionID -> SessionM Emitter
flushBuffer sessionID = readTable $ \table -> do
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just (Session _ chan) -> readChan chan
        Nothing               -> return NoEmitter