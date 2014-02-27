--------------------------------------------------------------------------------
-- | Session Layer
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
-- | The final stage
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
            debugSession Info $ "Emit: " <> serialize eventName
            return $ MsgEvent NoID NoEndpoint (Event eventName payloads)
        -- broadcast
        Just (Broadcast _, Event eventName payloads) -> do
            -- this log will cause massive overhead, need to be removed
            debugSession Info $ "Broadcast: " <> serialize eventName
            return $ MsgEvent NoID NoEndpoint (Event eventName payloads)
        -- wtf
        Just (_, NoEvent) -> do
            debugSession Error $ "Event malformed"
            return $ MsgEvent NoID NoEndpoint NoEvent
        -- no output, keep polling
        Nothing -> do
            return MsgNoop

handleSession (SessionEmit event) = do
    case event of
        Event eventName _ -> debugSession Info $ "On: " <> serialize eventName
        NoEvent           -> debugSession Error $ "Event malformed"
    triggerEvent event
    return $ MsgConnect NoEndpoint

handleSession SessionDisconnectByClient = do
    debugSession Info $ "Disconnected by client"
    triggerEvent (Event "disconnect" [])
    return $ MsgNoop

handleSession SessionDisconnectByServer = do
    debugSession Info $ "Disconnected by server"
    triggerEvent (Event "disconnect" [])
    return $ MsgNoop

--------------------------------------------------------------------------------
-- | Trigger corresponding listeners
triggerEvent :: Event -> SessionM ()
triggerEvent (Event eventName payload) = do

    sessionID <- getSessionID
    channelHub <- getChannelHub

    -- read
    listeners <- getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) eventName . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        _ <- liftIO $ runReaderT (execWriterT (runCallbackM callback)) (CallbackEnv eventName payload channelHub sessionID)
        return ()
triggerEvent NoEvent = error "triggering malformed event"

--------------------------------------------------------------------------------
-- | Wrapper
runSession :: SessionAction -> Session -> ConnectionM Message
runSession action session = runReaderT (runSessionM (handleSession action)) session