--------------------------------------------------------------------------------
-- | Requests are comsumed and sessions are invoked at this stage.
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Connection
    (   runConnection
    ,   newSessionTableRef
    )  where

--------------------------------------------------------------------------------
import              Web.SocketIO.Channel
import              Web.SocketIO.Session
import              Web.SocketIO.Timeout
import              Web.SocketIO.Types
import              Web.SocketIO.Log

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>))
import              Control.Concurrent.MVar.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Writer       
import qualified    Data.HashMap.Strict                     as H
import              Data.IORef.Lifted
import              System.Random                           (randomRIO)

--------------------------------------------------------------------------------
--
--  # State transitions after handshake
--
--                  Connect     Disconnect      Emit            Timeout
--  ----------------------------------------------------------------------------
--
--  Connecting      Connected   Disconnected    Disconnected    Disconnected
--                  (OK)        (OK)            (Error)         (OK)
--                              x               
--  Connected       Connected   Disconnected    Connected       Disconnected
--                  (OK)        (OK)            (OK)            (OK)
--                  Noop        x
--------------------------------------------------------------------------------
-- | Initialize session table
newSessionTableRef :: IO (IORef SessionTable)
newSessionTableRef = newIORef H.empty

--------------------------------------------------------------------------------
-- | Session table helper function
updateSession :: (SessionTable -> SessionTable) -> ConnectionM ()
updateSession update = do
    tableRef <- getSessionTableRef
    liftIO (modifyIORef tableRef update)

--------------------------------------------------------------------------------
-- | Session table helper function
lookupSession :: SessionID -> ConnectionM (Maybe Session)
lookupSession sessionID = do
    tableRef <- getSessionTableRef
    table <- liftIO (readIORef tableRef)
    return (H.lookup sessionID table)

--------------------------------------------------------------------------------
-- | Runs handler whenever new session comes in
executeHandler :: HandlerM () -> ChannelHub -> SessionID -> ConnectionM [Listener]
executeHandler handler channelHub sessionID = liftIO $ execWriterT (runReaderT (runHandlerM handler) (HandlerEnv channelHub sessionID))

--------------------------------------------------------------------------------
-- | Consumes request, hand it to the next stage, and returns its result.
runConnection :: Env -> Req -> IO Message
runConnection env req = do
    runReaderT (runConnectionM (handleConnection req)) env

--------------------------------------------------------------------------------
-- | Explicitly exposes `Web.SocketIO.Types.Base.SessionState` and make use of totality checker.
split :: Maybe Session -> Maybe (Session, SessionState)
split (Just session@(Session _ state _ _ _)) = Just (session, state)
split Nothing = Nothing

--------------------------------------------------------------------------------
-- | Accesses the session table and makes session explicit
retrieveSession :: Request -> ConnectionM (Request, Maybe (Session, SessionState))
retrieveSession Handshake = do
    return (Handshake, Nothing)
retrieveSession (Connect sessionID) = do
    result <- lookupSession sessionID
    return (Connect sessionID, split result)
retrieveSession (Disconnect sessionID) = do
    result <- lookupSession sessionID
    return (Disconnect sessionID, split result)
retrieveSession (Request sessionID e) = do
    result <- lookupSession sessionID
    return (Request sessionID e, split result)

--------------------------------------------------------------------------------
-- | Big time
handleConnection :: Req -> ConnectionM Message
handleConnection req = do



    return undefined

--    -- establish a session
--    session@(Session sessionID _ _ _ _) <- makeSession

--    -- session table management
--    updateSession (H.insert sessionID session)
--    logWithSessionID Debug sessionID "[Session] Created"
--    logWithSessionID Debug sessionID "[Request] Handshake"
    
--    -- timeout
--    setTimeout session

--    -- next stage
--    runSession SessionHandshake session


--    where   genSessionID = liftIO $ serialize <$> randomRIO (100000000000, 999999999999 :: Integer)
--            makeSession = do

--                sessionID <- genSessionID
--                channelHub <- makeChannelHub sessionID
--                handler <- getHandler
--                listeners <- executeHandler handler channelHub sessionID
--                timeoutVar <- newEmptyMVar

--                return $ Session sessionID Connecting channelHub listeners timeoutVar

--handleConnection (Connect sessionID, Just (session, Connecting)) = do
--    logWithSessionID Debug sessionID "[Request] Connect: ACK"
--    extendTimeout session
--    let session' = session { sessionState = Connected }
--    updateSession (H.insert sessionID session')
--    runSession SessionConnect session'

--handleConnection (Connect sessionID, Just (session, Connected)) = do
--    logWithSessionID Debug sessionID "[Request] Connect: Polling"
--    extendTimeout session  
--    runSession SessionPolling session

--handleConnection (Connect sessionID, Nothing) = do
--    logWithSessionID Warn sessionID "[Request] Connect: Session not found" 
--    return $ MsgError NoEndpoint NoData

--handleConnection (Disconnect sessionID, Just (session, _)) = do 
--    logWithSessionID Debug sessionID "[Request] Disconnect: By client"
--    clearTimeout session
--    updateSession (H.delete sessionID)
--    logWithSessionID Debug sessionID "[Session] Destroyed"
--    runSession SessionDisconnectByClient session

--handleConnection (Disconnect sessionID, Nothing) = do
--    logWithSessionID Warn sessionID "[Request] Disconnect: Session not found" 
--    return MsgNoop

--handleConnection (Request sessionID _, Just (session, Connecting)) = do
--    extendTimeout session
--    logWithSessionID Warn sessionID "[Request] Request: Session still connecting, not ACKed" 
--    return $ MsgError NoEndpoint NoData


--handleConnection (Request sessionID (MsgEvent _ _ event@(Event eventName (Payload_ payloads))), Just (session, Connected)) = do
--    logWithSessionID Debug sessionID $ "[Request] Request: MsgEvent " <> serialize eventName <> " " <> serialize payloads
--    runSession (SessionEmit event) session

--handleConnection (Request sessionID (MsgEvent _ _ NoEvent), Just (_, Connected)) = do
--    logWithSessionID Warn sessionID "[Request] Request: MsgEvent malformed"
--    return $ MsgError NoEndpoint NoData

--handleConnection (Request sessionID message, Just (_, Connected)) = do
--    logWithSessionID Debug sessionID $ "[Request] Request: " <> serialize message
--    return MsgNoop
--    --runSession (SessionEmit event) session


----handleConnection (Request sessionID event@(Event eventName (Payload payloads)), Just (session, Connected)) = do
----    logWithSessionID Debug sessionID $ "[Request] Request: " <> serialize eventName <> " " <> serialize payloads
----    runSession (SessionEmit event) session

----handleConnection (Request sessionID NoEvent, Just (_, Connected)) = do
----    logWithSessionID Warn sessionID "[Request] Request: malformed"
----    return $ MsgError NoEndpoint NoData

--handleConnection (Request sessionID _, Nothing) = do
--    logWithSessionID Warn sessionID "[Request] Request: Session not found" 
--    return $ MsgError NoEndpoint NoData