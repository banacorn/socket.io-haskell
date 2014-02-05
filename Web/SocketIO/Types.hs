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

--------------------------------------------------------------------------------
type Table = H.HashMap SessionID Session 
data Status = Connecting | Connected | Disconnecting deriving (Show, Eq)

--------------------------------------------------------------------------------
data SessionState   = SessionSyn
                    | SessionAck
                    | SessionPolling
                    | SessionEmit Emitter
                    | SessionDisconnect
                    | SessionError

--------------------------------------------------------------------------------
data Env = Env { 
    envSessionTable :: IORef Table, 
    envHandler :: HandlerM (), 
    envConfiguration :: Configuration,
    envStdout :: Chan String,
    envGlobalBuffer :: Buffer
}

--------------------------------------------------------------------------------
class ConnectionLayer m where
    getEnv :: m Env
    getSessionTable :: m (IORef Table)
    getHandler :: m (HandlerM ())
    getConfiguration :: m Configuration

--------------------------------------------------------------------------------
class SessionLayer m where
    getSession :: m Session
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
    getSessionTable = envSessionTable <$> ask
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
} | NoSession

instance Show Session where
    show (Session i s _ _ _) = "Session " 
                            ++ fromByteString i 
                            ++ " [" ++ show s ++ "]"
    show NoSession = "NoSession"

--------------------------------------------------------------------------------
newtype SessionM a = SessionM { runSessionM :: (ReaderT Session ConnectionM) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Session, MonadBase IO)

--------------------------------------------------------------------------------
instance ConnectionLayer SessionM where
    getEnv = SessionM (lift ask)
    getSessionTable = envSessionTable <$> getEnv
    getHandler = envHandler <$> getEnv
    getConfiguration = envConfiguration <$> getEnv

--------------------------------------------------------------------------------
instance SessionLayer SessionM where
    getSession = ask
    getSessionID = sessionSessionID <$> ask
    getStatus = sessionStatus <$> ask
    getBufferHub = sessionBufferHub <$> ask
    getLocalBuffer = selectLocalBuffer . sessionBufferHub <$> ask
    getGlobalBuffer = selectGlobalBuffer . sessionBufferHub <$> ask
    getListener = sessionListener <$> ask
    getTimeoutVar = sessionTimeoutVar <$> ask

--------------------------------------------------------------------------------
instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMSession { unStMSession :: StM (ReaderT Session ConnectionM) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMSession . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMSession
