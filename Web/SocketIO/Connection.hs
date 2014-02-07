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
    runReaderT (runConnectionM (handleConnection req)) env

--------------------------------------------------------------------------------
handleConnection :: Request -> ConnectionM ByteString
handleConnection Handshake = do
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

    runSession SessionSyn (Just session)
    where   genSessionID = liftIO $ fmap (fromString . show) (randomRIO (10000000000000000000, 99999999999999999999 :: Integer)) :: ConnectionM ByteString

handleConnection (Connect sessionID) = do

    result <- lookupSession sessionID
    clearTimeout sessionID
    case result of
        Just (Session sessionID' status buffer listeners timeout') -> do
            let session = Session sessionID' Connected buffer listeners timeout'
            case status of
                Connecting -> do
                    updateSession (H.insert sessionID' session)
                    runSession SessionAck (Just session)
                Connected ->
                    runSession SessionPolling (Just session)
                --Disconnected -> do
                --    debug . Error $ fromByteString sessionID' ++ "    Session Disconnected"
                --    runSession SessionError Nothing

        --Just NoSession -> do
        --    debug . Error $ fromByteString sessionID ++ "    No session" 
        --    runSession SessionError NoSession
        Nothing -> do
            debug . Error $ fromByteString sessionID ++ "    Unable to find session" 
            runSession SessionError Nothing

handleConnection (Disconnect sessionID) = do

    result <- lookupSession sessionID
    response <- case result of
        Just session -> runSession SessionDisconnect (Just session)
        Nothing -> return ""

    clearTimeout sessionID
    updateSession (H.delete sessionID)

    return response

handleConnection (Emit sessionID emitter) = do
    clearTimeout sessionID

    result <- lookupSession sessionID
    case result of
        Just session -> runSession (SessionEmit emitter) (Just session)
        Nothing      -> runSession SessionError Nothing

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