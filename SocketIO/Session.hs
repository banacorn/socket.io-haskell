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
import Control.Concurrent.MVar.Lifted
import Data.IORef.Lifted
import qualified Data.HashTable.IO as H

newTable :: IO (IORef Table)
newTable = H.new >>= newIORef

readTable :: (Table -> SessionM a) -> SessionM a
readTable f = ask >>= readIORef . getSessionTable >>= f

createSession :: SessionM SessionID
createSession = readTable $ \table -> do
    sessionID <- genSessionID
    buffer <- newEmptyMVar
    liftIO $ H.insert table sessionID $ Session Connecting buffer
    return sessionID
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

writeBuffer :: SessionID -> Text -> SessionM ()
writeBuffer sessionID text = readTable $ \table -> do
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just (Session _ bufferRef)  -> modifyMVar_ bufferRef (\buffer -> return text)
        Nothing                     -> return ()

flushBuffer :: SessionID -> SessionM Text
flushBuffer sessionID = readTable $ \table -> do
    result <- liftIO $ H.lookup table sessionID
    case result of
        Just (Session _ bufferRef)  -> readMVar bufferRef
        Nothing                     -> return ""