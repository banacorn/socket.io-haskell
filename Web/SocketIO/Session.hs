{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Session (runSession) where

import Web.SocketIO.Type
import Web.SocketIO.Util

import Control.Applicative          ((<$>), (<*>))
import Control.Monad.Reader       
import Control.Monad.Writer
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted    (fork)
import System.Timeout.Lifted

handleSession :: Configuration -> SessionState -> SessionM Text
handleSession config Syn = do
    sessionID <- getSessionID <$> ask
    debug $ "[Handshake]    " ++ fromText sessionID
    return $ sessionID <> ":60:60:xhr-polling"
handleSession config Ack = do
    sessionID <- getSessionID <$> ask
    debug $ "[Connecting]   " ++ fromText sessionID
    return "1::"
handleSession config Polling = do
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
handleSession config (Emit emitter) = do
    sessionID <- getSessionID <$> ask
    buffer <- getBuffer <$> ask
    debug $ "[Emit]         " ++ fromText sessionID
    triggerListener emitter buffer
    return "1"
handleSession config Disconnect = do
    debug $ "[Disconnect]   "
    return "1"
handleSession config Error = return "7"

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

runSession :: (Monad m, MonadIO m) => Configuration -> SessionState -> Session -> m Text
runSession config state session = liftIO $ runReaderT (runSessionM (handleSession config state)) session
