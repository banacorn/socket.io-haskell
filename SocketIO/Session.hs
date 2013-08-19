{-# LANGUAGE OverloadedStrings #-}
module SocketIO.Session where

import SocketIO.Type
import SocketIO.Util

import System.Random (randomRIO)
import Control.Applicative          ((<$>), (<*>))
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Data.IORef.Lifted
import qualified Data.HashTable.IO as H

newTableRef :: IO (IORef Table)
newTableRef = H.new >>= newIORef

newListenerListRef :: IO (IORef [Listener])
newListenerListRef = newIORef []

readTable :: (Table -> ConnectionM a) -> ConnectionM a
readTable f = ask >>= readIORef . getSessionTable >>= f

createSession :: SessionM a -> ConnectionM a
createSession f = do
    handler <- getHandler <$> ask
    readTable $ \table -> do
        sessionID <- genSessionID
        buffer <- newChan  
        listeners <- executeHandler handler buffer
        liftIO $ runReaderT (runSessionM f) (Session sessionID Connecting buffer listeners)

    where   genSessionID = liftIO $ fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: ConnectionM Text

--createSession :: SessionM SessionID
--createSession = readTable $ \table -> do
--    sessionID <- genSessionID
--    channel <- newChan
--    handler <- fmap getHandler ask
--    listeners <- executeHandler handler channel
--    registerListener listeners
--    liftIO $ H.insert table sessionID $ Session Connecting channel
--    return sessionID

--updateSession :: SessionID -> (Session -> SessionM Session) -> SessionM ()
--updateSession sessionID f = readTable $ \table -> do
--    result <- liftIO $ H.lookup table sessionID
--    liftIO $ H.delete table sessionID
--    case result of
--        Just session -> liftIO . H.insert table sessionID =<< f session
--        Nothing      -> return ()

--deleteSession :: SessionID -> SessionM ()
--deleteSession sessionID = readTable $ \table -> 
--    liftIO $ H.delete table sessionID

--lookupSession :: SessionID -> SessionM Session
--lookupSession sessionID = readTable $ \table -> do
--    result <- liftIO $ H.lookup table sessionID
--    case result of
--        Just session -> return session
--        Nothing      -> return NoSession

--writeBuffer :: SessionID -> Emitter -> SessionM ()
--writeBuffer sessionID emitter = readTable $ \table -> do
--    result <- liftIO $ H.lookup table sessionID
--    case result of
--        Just (Session _ chan)  -> writeChan chan emitter
--        Nothing                     -> return ()

--flushBuffer :: SessionID -> SessionM Emitter
--flushBuffer sessionID = readTable $ \table -> do
--    result <- liftIO $ H.lookup table sessionID
--    case result of
--        Just (Session _ chan) -> readChan chan
--        Nothing               -> return NoEmitter


executeHandler :: SocketM () -> Buffer -> ConnectionM [Listener]
executeHandler handler buffer = liftIO $ execWriterT (runReaderT (runSocketM handler) buffer)

--registerListener :: [Listener] -> SessionM ()
--registerListener listeners = ask >>= flip writeIORef listeners . getListener


--triggerListener :: Emitter -> Buffer -> SessionM ()
--triggerListener (Emitter event reply) channel = do
--    -- read
--    listeners <- ask >>= readIORef . getListener
--    -- filter out callbacks to be triggered
--    let correspondingCallbacks = filter ((==) event . fst) listeners
--    -- trigger them all
--    forM_ correspondingCallbacks $ \(_, callback) -> do
--        liftIO $ runReaderT (runReaderT (execWriterT (runCallbackM callback)) reply) channel