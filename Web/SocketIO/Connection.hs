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
import              Web.SocketIO.Util

--------------------------------------------------------------------------------
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
runConnection :: Env -> Request -> IO Message
runConnection env req = do
    runReaderT (runConnectionM (handleConnection =<< (retrieveSession req))) env

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
retrieveSession (Emit sessionID e) = do
    result <- lookupSession sessionID
    return (Emit sessionID e, split result)

--------------------------------------------------------------------------------
-- | Big time
handleConnection :: (Request, Maybe (Session, SessionState)) -> ConnectionM Message
handleConnection (Handshake, _) = do

    -- establish a session
    session@(Session sessionID _ _ _ _) <- makeSession

    -- session table management
    updateSession (H.insert sessionID session)

    debugLog Debug session "[Handshake]"
    
    -- timeout
    setTimeout session

    -- next stage
    runSession SessionHandshake session


    where   genSessionID = liftIO $ fmap (fromString . show) (randomRIO (10000000000000000000, 99999999999999999999 :: Integer)) :: ConnectionM ByteString
    
            makeSession = do

                sessionID <- genSessionID
                channelHub <- makeChannelHub sessionID
                handler <- getHandler
                listeners <- executeHandler handler channelHub sessionID
                timeoutVar <- newEmptyMVar

                return $ Session sessionID Connecting channelHub listeners timeoutVar

handleConnection (Connect sessionID, Just (session, Connecting)) = do
    debugLog Debug session "[Connect] Connecting"

    extendTimeout session

    let session' = session { sessionState = Connected }
    updateSession (H.insert sessionID session')
    runSession SessionConnect session'

handleConnection (Connect _, Just (session, Connected)) = do
    debugLog Debug session "[Connect] Polling"

    extendTimeout session
    
    runSession SessionPolling session

handleConnection (Connect sessionID, Nothing) = do
    debug Warn $ sessionID <> "    [Connect] Session not found" 
    return $ MsgError NoEndpoint NoData

handleConnection (Disconnect sessionID, Just (session, _)) = do
    
    debugLog Debug session "[Disconnect]"

    clearTimeout session

    updateSession (H.delete sessionID)
    runSession SessionDisconnectByClient session

handleConnection (Disconnect sessionID, Nothing) = do
    debug Warn $ sessionID <> "    [Disconnect] Session not found" 

    return MsgNoop

handleConnection (Emit _ _, Just (session, Connecting)) = do
    extendTimeout session

    debugLog Warn session "[Emit] Session still connecting" 
    return $ MsgError NoEndpoint NoData

handleConnection (Emit _ emitter, Just (session, Connected)) = do
    debugLog Debug session $ "[Emit] " <> serialize emitter

    extendTimeout session

    runSession (SessionEmit emitter) session

handleConnection (Emit _ _, Nothing) = do
    return $ MsgError NoEndpoint NoData