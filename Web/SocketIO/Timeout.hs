--------------------------------------------------------------------------------
-- | Timeout management
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Timeout
    (   setTimeout
    ,   extendTimeout
    ,   clearTimeout
    )  where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Log
import              Web.SocketIO.Session

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
-- | As microseconds
getTimeoutDuration :: ConnectionM Int
getTimeoutDuration = toMicroSec . closeTimeout <$> getConfiguration
    where   toMicroSec = (*) 1000000

--------------------------------------------------------------------------------
-- | The first parameter indicates whether it is first set timeout or not
primTimeout' :: Bool -> Session -> ConnectionM ()
primTimeout' firstTime session@(Session sessionID _ _ _ timeoutVar) = do

    duration <- getTimeoutDuration

    if firstTime 
        then debug Debug $ sessionID <> "    [Session] Set timeout"
        else debug Debug $ sessionID <> "    [Session] Extend timeout"
    result <- timeout duration $ takeMVar timeoutVar

    case result of
        -- extend!
        Just True -> primTimeout' False session
        -- die!
        Just False -> clearTimeout session
        Nothing -> do
            runSession SessionDisconnectByServer session
            -- remove session
            tableRef <- getSessionTableRef
            liftIO (modifyIORef tableRef (H.delete sessionID))
            debug Debug $ sessionID <> "    [Session] Destroyed: close timeout"

----------------------------------------------------------------------------------
-- | Set timeout
setTimeout :: Session -> ConnectionM ()
setTimeout = void . fork . primTimeout' True

----------------------------------------------------------------------------------
-- | Extend timeout
extendTimeout :: Session -> ConnectionM ()
extendTimeout (Session _ _ _ _ timeoutVar) = putMVar timeoutVar True

--------------------------------------------------------------------------------
-- | Clear timeout
clearTimeout :: Session -> ConnectionM ()
clearTimeout (Session _ _ _ _ timeoutVar) = do
    putMVar timeoutVar False

