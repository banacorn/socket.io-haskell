{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

--------------------------------------------------------------------------------
import Web.SocketIO.Types
import Web.SocketIO.Util

--------------------------------------------------------------------------------
import              Data.List                               (intersperse)
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Lifted        (fork)
import System.Timeout.Lifted

--------------------------------------------------------------------------------
handleSession :: SessionAction -> SessionM Message
handleSession SessionHandshake = do
    sessionID <- getSessionID
    configuration <- getConfiguration

    let heartbeatTimeout' = if heartbeats configuration
            then fromString (show (heartbeatTimeout configuration))
            else ""
    let closeTimeout' = fromString (show (closeTimeout configuration))
    let transportType = mconcat . intersperse "," . map serialize $ transports configuration

    return $ MsgRaw $ sessionID <> ":" <> heartbeatTimeout' <> ":" <> closeTimeout' <> ":" <> transportType
    

handleSession SessionConnect = do
    debugSession Info $ "Connected"
    return $ MsgConnect NoEndpoint

handleSession SessionPolling = do
    configuration <- getConfiguration
    channelHub <- getChannelHub
  
    result <- timeout (pollingDuration configuration * 1000000) (readBothChannel channelHub)

    case result of
        Just event@(Event eventName payloads) -> do
            debugSession Info $ "<<==  " <> serialize eventName <> " " <> serialize payloads
            return $ MsgEvent NoID NoEndpoint event
        Just NoEvent -> do
            debugSession Error $ "No Emit"
            return $ MsgEvent NoID NoEndpoint NoEvent
        Nothing -> do
            return MsgNoop

    where   readBothChannel (ChannelHub localChannel globalChannel _) = do
                output <- newEmptyMVar
                _ <- fork (readChan localChannel >>= putMVar output)
                _ <- fork (readChan globalChannel >>= putMVar output)
                
                takeMVar output



handleSession (SessionEmit event) = do
    channelHub <- getChannelHub

    case event of
        Event eventName payloads -> debugSession Info $ "==>>  " <> serialize eventName <> " " <> serialize payloads
        NoEvent                  -> debugSession Error $ "Event malformed"

    triggerListener event channelHub
    return $ MsgConnect NoEndpoint
handleSession SessionDisconnect = do
    debugSession Info $ "Disconnected"
    channelHub <- getChannelHub
    triggerListener (Event "disconnect" []) channelHub
    return $ MsgNoop

--------------------------------------------------------------------------------
triggerListener :: Event -> ChannelHub -> SessionM ()
triggerListener (Event event payload) channelHub = do
    -- read
    listeners <- getListener
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        _ <- liftIO $ runReaderT (execWriterT (runCallbackM callback)) (CallbackEnv payload channelHub)
        return ()
triggerListener NoEvent _ = error "trigger listeners with any events"

--------------------------------------------------------------------------------
runSession :: SessionAction -> Session -> ConnectionM Message
runSession action session = runReaderT (runSessionM (handleSession action)) session