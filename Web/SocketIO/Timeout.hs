{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Timeout
    (   setTimeout
    ,   extendTimeout
    ,   clearTimeout
    )  where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Util

--------------------------------------------------------------------------------
import qualified    Data.HashMap.Strict                     as H
import              Data.IORef.Lifted
import              Control.Applicative                     ((<$>))
import              Control.Concurrent.Lifted               (fork)
import              Control.Concurrent.MVar.Lifted
import              Control.Monad.Trans                     (liftIO)       
import              Control.Monad                           (void)       
import              System.Timeout.Lifted                   (timeout)

--------------------------------------------------------------------------------
getTimeoutDuration :: ConnectionM Int
getTimeoutDuration = toMicroSec . closeTimeout <$> getConfiguration
    where   toMicroSec = (*) 1000000

--------------------------------------------------------------------------------
extendTimeout' :: Bool -> Session -> ConnectionM ()
extendTimeout' firstTime session@(Session sessionID _ _ _ timeoutVar) = do

    duration <- getTimeoutDuration

    if firstTime 
        then debug Debug $ sessionID <> "    Set Timeout"
        else debug Debug $ sessionID <> "    Extend Timeout"
    result <- timeout duration $ takeMVar timeoutVar

    case result of
        -- extend!
        Just True -> extendTimeout' False session
        -- die!
        Just False -> clearTimeout session
        Nothing -> do
            debug Debug $ sessionID <> "    Close Session"
            debug Info $ sessionID <> "    Disconnected by server"
            -- remove session
            tableRef <- getSessionTableRef
            liftIO (modifyIORef tableRef (H.delete sessionID))
----------------------------------------------------------------------------------
setTimeout :: Session -> ConnectionM ()
setTimeout = void . fork . extendTimeout' True

extendTimeout :: Session -> ConnectionM ()
extendTimeout (Session _ _ _ _ timeoutVar) = do
    putMVar timeoutVar True


--------------------------------------------------------------------------------
clearTimeout :: Session -> ConnectionM ()
clearTimeout (Session sessionID _ _ _ timeoutVar) = do
    debug Debug $ sessionID <> "    Clear Timeout"
    putMVar timeoutVar False

