{-# LANGUAGE OverloadedStrings #-}
module SocketIO.Session (runSession) where

import SocketIO.Type
import SocketIO.Util

import Control.Applicative          ((<$>), (<*>))
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted    (fork)
import System.Timeout.Lifted

handleSession :: SessionState -> SessionM Text
handleSession Syn = do
    sessionID <- getSessionID <$> ask
    debug $ "[Handshake]    " ++ fromText sessionID
    return $ sessionID <> ":60:60:xhr-polling"
handleSession Ack = do
    sessionID <- getSessionID <$> ask
    debug $ "[Connecting]   " ++ fromText sessionID
    return "1::"
handleSession Polling = do
    sessionID <- getSessionID <$> ask
    buffer <- getBuffer <$> ask
    result <- timeout (20 * 1000000) (readChan buffer)
    case result of
        Just r  -> do
            debug $ "[Polling]*     " ++ fromText sessionID
            return $ toMessage (MsgEvent NoID NoEndpoint r)
        Nothing -> do
            debug $ "[Polling]      " ++ fromText sessionID
            return "8::"
handleSession (Emit emitter) = do
    sessionID <- getSessionID <$> ask
    buffer <- getBuffer <$> ask
    debug $ "[Emit]         " ++ fromText sessionID
    triggerListener emitter buffer
    return "1"
handleSession Disconnect = do
    debug $ "[Disconnect]   "
    return "1"
handleSession Error = return "7"

triggerListener :: Emitter -> Buffer -> SessionM ()
triggerListener (Emitter event reply) channel = do
    -- read
    listeners <- getListener <$> ask
    -- filter out callbacks to be triggered
    let correspondingCallbacks = filter ((==) event . fst) listeners
    -- trigger them all
    forM_ correspondingCallbacks $ \(_, callback) -> fork $ do
        liftIO $ runReaderT (runReaderT (execWriterT (runCallbackM callback)) reply) channel
        return ()

runSession :: (Monad m, MonadIO m) => SessionState -> Session -> m Text
runSession state session = liftIO $ runReaderT (runSessionM (handleSession state)) session
