module SocketIO.Session (
    createSession, 
    updateSession, 
    deleteSession, 
    lookupSession,
    newTable
) where

import SocketIO.Type
import SocketIO.Util

import System.Random (randomRIO)
import Control.Monad.Reader       
import Data.IORef
import qualified Data.HashTable.IO as H

newTable :: IO (IORef Table)
newTable = H.new >>= newIORef

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