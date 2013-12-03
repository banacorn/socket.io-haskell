{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

import Web.SocketIO.Types
import Web.SocketIO.Util

import Data.List (intersperse)
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted    (fork)
import System.Timeout.Lifted

handleSession :: SessionState -> SessionM Text
handleSession SessionSyn = do
    sessionID <- getSessionID
    configuration <- getConfiguration

    let heartbeatTimeout' = fromString (show (heartbeatTimeout configuration))
    let closeTimeout' = fromString (show (closeTimeout configuration))
    let transportType = mconcat . intersperse "," . map toMessage $ transports configuration



    debug . Info $ fromText sessionID ++ "    Handshake authorized"
    return $ sessionID <> ":" <> heartbeatTimeout' <> ":" <> closeTimeout' <> ":" <> transportType


handleSession SessionAck = do
    sessionID <- getSessionID
    debug . Info $ fromText sessionID ++ "    Connected"
    return "1::"

handleSession SessionPolling = do
    sessionID <- getSessionID
    configuration <- getConfiguration
    buffer <- getBuffer
    result <- timeout (pollingDuration configuration * 1000000) (readChan buffer)
    case result of
        Just r  -> do
            debug . Debug $ fromText sessionID ++ "    Polling*"
            return $ toMessage (MsgEvent NoID NoEndpoint r)
        Nothing -> do
            debug . Debug $ fromText sessionID ++ "    Polling"
            return "8::"
handleSession (SessionEmit emitter) = do
    sessionID <- getSessionID
    buffer <- getBuffer
    debug . Debug $ fromText sessionID ++ "    Emit"
    triggerListener emitter buffer
    return "1"
handleSession SessionDisconnect = do
    debug . Info $ "             Disconnected"
    return "1"
handleSession SessionError = return "7"

triggerListener :: Emitter -> Buffer -> SessionM ()
triggerListener (Emitter event reply) channel = do
    -- read
    listeners <- getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        _ <- liftIO $ runReaderT (runReaderT (execWriterT (runCallbackM callback)) reply) channel
        return ()
triggerListener NoEmitter _ = return ()

runSession :: SessionState -> Session -> ConnectionM Text
runSession state session = runReaderT (runSessionM (handleSession state)) session