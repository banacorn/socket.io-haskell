{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

--------------------------------------------------------------------------------
import Web.SocketIO.Types
import Web.SocketIO.Util

--------------------------------------------------------------------------------
import Data.List                        (intersperse)
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Lifted        (fork)
import System.Timeout.Lifted

--------------------------------------------------------------------------------
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
    bufferHub <- getBufferHub
  
    result <- timeout (pollingDuration configuration * 1000000) (readBothChannel bufferHub)
    case result of
        Just r  -> do
            let msg = toMessage (MsgEvent NoID NoEndpoint r)
            debug . Debug $ fromText sessionID ++ "    Sending Message: " ++ fromText msg
            return msg
        Nothing -> do
            debug . Debug $ fromText sessionID ++ "    Polling"
            return "8::"

    where   readBothChannel (BufferHub localBuffer globalBuffer) = do
                output <- newEmptyMVar
                _ <- fork (readChan localBuffer >>= putMVar output)
                _ <- fork (readChan globalBuffer >>= putMVar output)
                
                takeMVar output



handleSession (SessionEmit emitter) = do
    sessionID <- getSessionID
    bufferHub <- getBufferHub
    debug . Info $ fromText sessionID ++ "    Emit"
    triggerListener emitter bufferHub
    return "1"
handleSession SessionDisconnect = do
    sessionID <- getSessionID
    debug . Info $ fromText sessionID ++ "    Disconnected"
    bufferHub <- getBufferHub
    triggerListener (Emitter "disconnect" []) bufferHub
    return "1"
handleSession SessionError = return "7"

--------------------------------------------------------------------------------
triggerListener :: Emitter -> BufferHub -> SessionM ()
triggerListener (Emitter event payload) channelHub = do
    -- read
    listeners <- getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        _ <- liftIO $ runReaderT (execWriterT (runCallbackM callback)) (CallbackEnv payload channelHub)
        return ()
triggerListener NoEmitter _ = error "trigger listeners with any emitters"

--------------------------------------------------------------------------------
runSession :: SessionState -> Session -> ConnectionM Text
runSession state session = runReaderT (runSessionM (handleSession state)) session