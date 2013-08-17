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
import Control.Concurrent.MVar       
import Data.IORef
import qualified Data.HashTable.IO as H

newTable :: IO (IORef Table)
newTable = H.new >>= newIORef

readTable :: (Table -> IO a) -> SessionM a
readTable f = ask >>= liftIO . readIORef . getSessionTable >>= liftIO . f

createSession :: SessionM SessionID
createSession = readTable $ \table -> do
    sessionID <- genSessionID
    buffer <- newEmptyMVar
    H.insert table sessionID $ Session Connecting buffer
    return sessionID
    where   genSessionID = fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: IO Text

updateSession :: SessionID -> (Session -> IO Session) -> SessionM ()
updateSession sessionID f = readTable $ \table -> do
    result <- H.lookup table sessionID
    H.delete table sessionID
    case result of
        Just session -> H.insert table sessionID =<< f session
        Nothing      -> return ()

deleteSession :: SessionID -> SessionM ()
deleteSession sessionID = readTable $ \table -> 
    H.delete table sessionID

lookupSession :: SessionID -> SessionM Session
lookupSession sessionID = readTable $ \table -> do
    result <- H.lookup table sessionID
    case result of
        Just session -> return session
        Nothing      -> return NoSession

writeBuffer :: SessionID -> Text -> SessionM ()
writeBuffer sessionID text = readTable $ \table -> do
    result <- H.lookup table sessionID
    case result of
        Just (Session _ bufferRef)  -> modifyMVar_ bufferRef (\buffer -> return text)
        Nothing                     -> return ()

flushBuffer :: SessionID -> SessionM Text
flushBuffer sessionID = readTable $ \table -> do
    result <- H.lookup table sessionID
    case result of
        Just (Session _ bufferRef)  -> readMVar bufferRef
        Nothing                     -> return ""