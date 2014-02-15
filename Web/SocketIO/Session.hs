{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

--------------------------------------------------------------------------------
import Web.SocketIO.Types
import Web.SocketIO.Util

--------------------------------------------------------------------------------
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted        (fork)
import System.Timeout.Lifted

--------------------------------------------------------------------------------
handleSession :: SessionAction -> SessionM Message
handleSession SessionHandshake = do
    sessionID <- getSessionID
    configuration <- getConfiguration

    let heartbeatTimeout' = if heartbeats configuration
            then heartbeatTimeout configuration
            else 0

    return $ MsgHandshake
                sessionID
                heartbeatTimeout'
                (closeTimeout configuration)
                (transports configuration)
    

handleSession SessionConnect = do
    debugSession Info $ "Connected"
    return $ MsgConnect NoEndpoint

handleSession SessionPolling = do
    configuration <- getConfiguration
    ChannelHub _ _ outputChannel _ <- getChannelHub
  
    result <- timeout (pollingDuration configuration * 1000000) (readChan outputChannel)

    case result of
        -- private
        Just (Private, Event eventName payloads) -> do
            debugSession Info $ "--->  " <> serialize eventName <> " " <> serialize payloads
            return $ MsgEvent NoID NoEndpoint (Event eventName payloads)
        -- broadcast
        Just (Broadcast _, Event eventName payloads) -> do
            -- this log will cause massive overhead, need to be removed
            debugSession Info $ "*-->  " <> serialize eventName <> " " <> serialize payloads
            return $ MsgEvent NoID NoEndpoint (Event eventName payloads)
        -- wtf
        Just (_, NoEvent) -> do
            debugSession Error $ "No Emit"
            return $ MsgEvent NoID NoEndpoint NoEvent
        -- no output, keep polling
        Nothing -> do
            return MsgNoop

handleSession (SessionEmit event) = do
    sessionID <- getSessionID
    channelHub <- getChannelHub
    case event of
        Event eventName payloads -> debugSession Info $ "<---  " <> serialize eventName <> " " <> serialize payloads
        NoEvent                  -> debugSession Error $ "Event malformed"

    triggerListener event channelHub sessionID
    return $ MsgConnect NoEndpoint
handleSession SessionDisconnect = do
    sessionID <- getSessionID
    debugSession Info $ "Disconnected by client"
    channelHub <- getChannelHub
    triggerListener (Event "disconnect" []) channelHub sessionID
    return $ MsgNoop

--------------------------------------------------------------------------------
triggerListener :: Event -> ChannelHub -> SessionID -> SessionM ()
triggerListener (Event event payload) channelHub sessionID = do
    -- read
    listeners <- getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        _ <- liftIO $ runReaderT (execWriterT (runCallbackM callback)) (CallbackEnv payload channelHub sessionID)
        return ()
triggerListener NoEvent _ _ = error "trigger listeners with any events"

--------------------------------------------------------------------------------
runSession :: SessionAction -> Session -> ConnectionM Message
runSession action session = runReaderT (runSessionM (handleSession action)) session