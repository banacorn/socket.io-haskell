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
import qualified    Data.HashMap.Strict                     as H
import              Data.IORef.Lifted
import              Control.Concurrent.MVar.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Writer       
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
newSessionTableRef :: IO (IORef Table)
newSessionTableRef = newIORef H.empty

--------------------------------------------------------------------------------
updateSession :: (Table -> Table) -> ConnectionM ()
updateSession update = do
    tableRef <- getSessionTableRef
    liftIO (modifyIORef tableRef update)

--------------------------------------------------------------------------------
lookupSession :: SessionID -> ConnectionM (Maybe Session)
lookupSession sessionID = do
    tableRef <- getSessionTableRef
    table <- liftIO (readIORef tableRef)
    return (H.lookup sessionID table)

--------------------------------------------------------------------------------
executeHandler :: HandlerM () -> ChannelHub -> SessionID -> ConnectionM [Listener]
executeHandler handler channelHub sessionID = liftIO $ execWriterT (runReaderT (runHandlerM handler) (HandlerEnv channelHub sessionID))

--------------------------------------------------------------------------------
runConnection :: Env -> Request -> IO Message
runConnection env req = do
    runReaderT (runConnectionM (handleConnection =<< (retrieveSession req))) env

--------------------------------------------------------------------------------
split :: Maybe Session -> Maybe (Session, SessionState)
split (Just session@(Session _ state _ _ _)) = Just (session, state)
split Nothing = Nothing

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
handleConnection :: (Request, Maybe (Session, SessionState)) -> ConnectionM Message
handleConnection (Handshake, _) = do



    session@(Session sessionID _ _ _ _) <- makeSession
    setTimeout session
    debugLog Debug session "[Handshake]"

    updateSession (H.insert sessionID session)
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
    runSession SessionDisconnect session

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