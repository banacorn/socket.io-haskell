{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Connection
    (   runConnection
    ,   newSessionTableRef
    )  where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Util
import              Web.SocketIO.Session

--------------------------------------------------------------------------------
import qualified    Data.HashMap.Strict                     as H
import              Data.IORef.Lifted
import              Control.Applicative                     ((<$>))
import              Control.Concurrent.Lifted               (fork)
import              Control.Concurrent.Chan.Lifted 
import              Control.Concurrent.MVar.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Writer       
import              System.Random                           (randomRIO)
import              System.Timeout.Lifted

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
executeHandler :: HandlerM () -> BufferHub -> ConnectionM [Listener]
executeHandler handler bufferHub = liftIO $ execWriterT (runReaderT (runHandlerM handler) bufferHub)

--------------------------------------------------------------------------------
runConnection :: Env -> Request -> IO ByteString
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
handleConnection :: (Request, Maybe (Session, SessionState)) -> ConnectionM ByteString
handleConnection (Handshake, _) = do
    globalBuffer <- envGlobalBuffer <$> getEnv
    globalBufferClone <- dupChan globalBuffer
    localBuffer <- newChan
    let bufferHub = BufferHub localBuffer globalBufferClone
    handler <- getHandler
    sessionID <- genSessionID
    listeners <- executeHandler handler bufferHub
    timeout' <- newEmptyMVar

    let session = Session sessionID Connecting bufferHub listeners timeout'

    _ <- fork $ setTimeout sessionID timeout'

    updateSession (H.insert sessionID session)

    runSession SessionHandshake session
    where   genSessionID = liftIO $ fmap (fromString . show) (randomRIO (10000000000000000000, 99999999999999999999 :: Integer)) :: ConnectionM ByteString

handleConnection (Connect sessionID, Just (session, Connecting)) = do
    clearTimeout sessionID

    let session' = session { sessionState = Connected }
    updateSession (H.insert sessionID session')
    runSession SessionConnect session'

handleConnection (Connect sessionID, Just (session, Connected)) = do
    clearTimeout sessionID
    
    runSession SessionPolling session

handleConnection (Connect sessionID, Nothing) = do
    debug . Error $ fromByteString sessionID ++ "    Session not found" 
    return "7"

handleConnection (Disconnect sessionID, Just (session, _)) = do

    clearTimeout sessionID

    updateSession (H.delete sessionID)
    runSession SessionDisconnect session

handleConnection (Disconnect sessionID, Nothing) = do

    clearTimeout sessionID
    return ""

handleConnection (Emit sessionID _, Just (_, Connecting)) = do
    clearTimeout sessionID

    debug . Error $ fromByteString sessionID ++ "    Session still connecting" 
    return "7"

handleConnection (Emit sessionID emitter, Just (session, Connected)) = do

    clearTimeout sessionID

    runSession (SessionEmit emitter) session

handleConnection (Emit _ _, Nothing) = do
    return "7"
    --runSession SessionError Nothing

--------------------------------------------------------------------------------
setTimeout :: SessionID -> MVar () -> ConnectionM ()
setTimeout sessionID timeout' = do
    configuration <- getConfiguration
    let duration = (closeTimeout configuration) * 1000000
    debug . Debug $ fromByteString sessionID ++ "    Set Timeout"
    result <- timeout duration $ takeMVar timeout'

    case result of
        Just _  -> setTimeout sessionID timeout'
        Nothing -> do
            debug . Debug $ fromByteString sessionID ++ "    Close Session"
            updateSession (H.delete sessionID)

--------------------------------------------------------------------------------
clearTimeout :: SessionID -> ConnectionM ()
clearTimeout sessionID = do
    result <- lookupSession sessionID
    case result of
        Just (Session _ _ _ _ timeout') -> do
            debug . Debug $ fromByteString sessionID ++ "    Clear Timeout"
            putMVar timeout' ()
        Nothing                         -> return ()