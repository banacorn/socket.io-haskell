--------------------------------------------------------------------------------
-- | Session Layer
{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

--------------------------------------------------------------------------------
import Web.SocketIO.Types
import Web.SocketIO.Log

--------------------------------------------------------------------------------
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted        (fork)
import System.Timeout.Lifted
import Data.HashMap.Strict (fromList)
import Data.Vector (empty)
import Data.Aeson (Value(..), encode)
--------------------------------------------------------------------------------
-- | The final stage
handleSession :: SessionAction -> SessionM Packet
handleSession SessionOpen = do
    sessionID <- getSessionID
    configuration <- getConfiguration

    let value = Object $ fromList 
            [   ("sid"          , String $ serialize sessionID)
            ,   ("upgrades"     , Array  $ empty)
            ,   ("pingInterval" , Number $ fromInteger 25000)
            ,   ("pingTimeout"  , Number $ fromInteger 60000)
            ]

    return (Packet Open (serialize $ encode value))

--handleSession SessionConnect = do
--    logWithSession Info $ "Connected"
--    triggerEvent (Event "connection" (Payload_ []))
--    return $ MsgConnect NoEndpoint

--handleSession SessionPolling = do
--    configuration <- getConfiguration
--    ChannelHub _ _ outputChannel _ <- getChannelHub
  
--    result <- timeout (pollingDuration configuration * 1000000) (readChan outputChannel)

--    case result of
--        -- private
--        Just (Private, Event eventName payloads) -> do
--            logWithSession Info $ "Emit: " <> serialize eventName
--            return $ MsgEvent NoID NoEndpoint (Event eventName payloads)
--        -- broadcast
--        Just (Broadcast _, Event eventName payloads) -> do
--            -- this log will cause massive overhead, need to be removed
--            logWithSession Info $ "Broadcast: " <> serialize eventName
--            return $ MsgEvent NoID NoEndpoint (Event eventName payloads)
--        -- wtf
--        Just (_, NoEvent) -> do
--            logWithSession Error $ "Event malformed"
--            return $ MsgEvent NoID NoEndpoint NoEvent
--        -- no output, keep polling
--        Nothing -> do
--            return MsgNoop

--handleSession (SessionEmit event) = do
--    case event of
--        Event eventName _ -> logWithSession Info $ "On: " <> serialize eventName
--        NoEvent           -> logWithSession Error $ "Event malformed"
--    triggerEvent event
--    return $ MsgConnect NoEndpoint

--handleSession SessionDisconnectByClient = do
--    logWithSession Info $ "Disconnected by client"
--    triggerEvent (Event "disconnect" (Payload_ []))
--    return $ MsgNoop

--handleSession SessionDisconnectByServer = do
--    logWithSession Info $ "Disconnected by server"
--    triggerEvent (Event "disconnect" (Payload_ []))
--    return $ MsgNoop

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
runSession :: SessionAction -> Session -> ConnectionM Packet
runSession action session = runReaderT (runSessionM (handleSession action)) session