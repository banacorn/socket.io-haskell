{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Web.SocketIO.Types
    (   module Web.SocketIO.Types.Log
    ,   module Web.SocketIO.Types.Request
    ,   module Web.SocketIO.Types.SocketIO
    ,   module Web.SocketIO.Types.String
    ,   ConnectionM(..)
    ,   SessionM(..)
    ,   ConnectionLayer(..)
    ,   SessionLayer(..)
    ,   Env(..)
    ,   Session(..)
    ,   SessionState(..)
    ,   Status(..)
    ,   Table
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.Request
import              Web.SocketIO.Types.Log
import              Web.SocketIO.Types.String
import              Web.SocketIO.Types.SocketIO

--------------------------------------------------------------------------------
import              Control.Applicative
import              Control.Concurrent.MVar.Lifted
import              Control.Concurrent.Chan.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Trans.Control
import              Control.Monad.Base

import qualified    Data.HashMap.Strict                     as H
import              Data.IORef.Lifted
import              Data.Maybe                              (fromJust)

--------------------------------------------------------------------------------
type Table = H.HashMap SessionID Session 
data Status = Connecting | Connected | Disconnected deriving (Show, Eq)

--------------------------------------------------------------------------------
data SessionState   = SessionSyn
                    | SessionAck
                    | SessionPolling
                    | SessionEmit Emitter
                    | SessionDisconnect
                    | SessionError

--------------------------------------------------------------------------------
data Env = Env { 
    envSessionTableRef :: IORef Table, 
    envHandler :: HandlerM (), 
    envConfiguration :: Configuration,
    envStdout :: Chan String,
    envGlobalBuffer :: Buffer
}

--------------------------------------------------------------------------------
class ConnectionLayer m where
    getEnv :: m Env
    getSessionTableRef :: m (IORef Table)
    getHandler :: m (HandlerM ())
    getConfiguration :: m Configuration

--------------------------------------------------------------------------------
class SessionLayer m where
    getSession :: m (Maybe Session)
    getSessionID :: m SessionID
    getStatus :: m Status
    getBufferHub :: m BufferHub
    getLocalBuffer :: m Buffer
    getGlobalBuffer :: m Buffer
    getListener :: m [Listener]
    getTimeoutVar :: m (MVar ())

--------------------------------------------------------------------------------
newtype ConnectionM a = ConnectionM { runConnectionM :: ReaderT Env IO a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Env, MonadBase IO)

--------------------------------------------------------------------------------
instance ConnectionLayer ConnectionM where
    getEnv = ask
    getSessionTableRef = envSessionTableRef <$> ask
    getHandler = envHandler <$> ask
    getConfiguration = envConfiguration <$> ask

--------------------------------------------------------------------------------
instance (MonadBaseControl IO) ConnectionM where
    newtype StM ConnectionM a = StMConnection { unStMConnection :: StM (ReaderT Env IO) a }
    liftBaseWith f = ConnectionM (liftBaseWith (\run -> f (liftM StMConnection . run . runConnectionM)))
    restoreM = ConnectionM . restoreM . unStMConnection

--------------------------------------------------------------------------------
data Session = Session { 
    sessionSessionID :: SessionID, 
    sessionStatus :: Status, 
    sessionBufferHub :: BufferHub, 
    sessionListener :: [Listener],
    sessionTimeoutVar :: MVar ()
}

instance Show Session where
    show (Session i s _ _ _) = "Session " 
                            ++ fromByteString i 
                            ++ " [" ++ show s ++ "]"

--------------------------------------------------------------------------------
newtype SessionM a = SessionM { runSessionM :: (ReaderT (Maybe Session) ConnectionM) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (Maybe Session), MonadBase IO)

--------------------------------------------------------------------------------
instance ConnectionLayer SessionM where
    getEnv = SessionM (lift ask)
    getSessionTableRef = envSessionTableRef <$> getEnv
    getHandler = envHandler <$> getEnv
    getConfiguration = envConfiguration <$> getEnv

--------------------------------------------------------------------------------
instance SessionLayer SessionM where
    getSession = ask
    getSessionID = sessionSessionID . fromJust <$> ask
    getStatus = sessionStatus . fromJust <$> ask
    getBufferHub = sessionBufferHub . fromJust <$> ask
    getLocalBuffer = selectLocalBuffer . sessionBufferHub . fromJust <$> ask
    getGlobalBuffer = selectGlobalBuffer . sessionBufferHub . fromJust <$> ask
    getListener = sessionListener . fromJust <$> ask
    getTimeoutVar = sessionTimeoutVar . fromJust <$> ask

--------------------------------------------------------------------------------
instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMSession { unStMSession :: StM (ReaderT (Maybe Session) ConnectionM) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMSession . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMSession
