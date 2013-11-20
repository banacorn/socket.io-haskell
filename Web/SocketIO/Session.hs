{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

import Web.SocketIO.Type
import Web.SocketIO.Type.String
import Web.SocketIO.Type.Log
import Web.SocketIO.Util

import Data.List (intersperse)
import Control.Applicative          ((<$>), (<*>))
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted    (fork)
import System.Timeout.Lifted

handleSession :: SessionState -> SessionM Text
handleSession Syn = do
    sessionID <- getSessionID
    configuration <- getConfiguration
    let transportType = mconcat . intersperse "," . map toMessage $ transports configuration
    debug . Info $ "[Handshake]    " ++ fromText sessionID
    return $ sessionID <> ":60:60:" <> transportType


handleSession Ack = do
    sessionID <- getSessionID
    debug . Info $ "[Connecting]   " ++ fromText sessionID
    return "1::"
handleSession Polling = do
    sessionID <- getSessionID
    buffer <- getBuffer
    result <- timeout (20 * 1000000) (readChan buffer)
    case result of
        Just r  -> do
            debug . Info $ "[Polling]*     " ++ fromText sessionID
            return $ toMessage (MsgEvent NoID NoEndpoint r)
        Nothing -> do
            debug . Info $ "[Polling]      " ++ fromText sessionID
            return "8::"
handleSession (Emit emitter) = do
    sessionID <- getSessionID
    buffer <- getBuffer
    debug . Info $ "[Emit]         " ++ fromText sessionID
    triggerListener emitter buffer
    return "1"
handleSession Disconnect = do
    debug . Info $ "[Disconnect]   "
    return "1"
handleSession Err = return "7"

triggerListener :: Emitter -> Buffer -> SessionM ()
triggerListener (Emitter event reply) channel = do
    -- read
    listeners <- getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        liftIO $ runReaderT (runReaderT (execWriterT (runCallbackM callback)) reply) channel
        return ()

runSession :: SessionState -> Session -> ConnectionM Text
runSession state session = runReaderT (runSessionM (handleSession state)) session