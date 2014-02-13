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
    ,   SessionAction(..)
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
data SessionState = Connecting | Connected deriving (Show, Eq)
data SessionAction   = SessionHandshake
                     | SessionConnect
                     | SessionPolling
                     | SessionEmit Event
                     | SessionDisconnect

--------------------------------------------------------------------------------
data Env = Env { 
    envSessionTableRef :: IORef Table, 
    envHandler :: HandlerM (), 
    envConfiguration :: Configuration,
    envLogChannel :: Chan ByteString,
    envGlobalChannel :: Chan Package
}

--------------------------------------------------------------------------------
class ConnectionLayer m where
    getEnv :: m Env
    getSessionTableRef :: m (IORef Table)
    getHandler :: m (HandlerM ())
    getConfiguration :: m Configuration

--------------------------------------------------------------------------------
class SessionLayer m where
    getSession :: m Session
    getSessionID :: m SessionID
    getSessionState :: m SessionState
    getChannelHub :: m ChannelHub
    --getLocalChannel :: m Buffer
    --getGlobalBuffer :: m Buffer
    getListener :: m [Listener]
    getTimeoutVar :: m (MVar Bool)

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
    sessionState :: SessionState, 
    sessionChannelHub :: ChannelHub, 
    sessionListener :: [Listener],
    sessionTimeoutVar :: MVar Bool
}

instance Show Session where
    show (Session i s _ _ _) = "Session " 
                            ++ fromByteString i 
                            ++ " [" ++ show s ++ "]"

--------------------------------------------------------------------------------
newtype SessionM a = SessionM { runSessionM :: (ReaderT Session ConnectionM) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Session, MonadBase IO)

--------------------------------------------------------------------------------
instance ConnectionLayer SessionM where
    getEnv = SessionM (lift ask)
    getSessionTableRef = envSessionTableRef <$> getEnv
    getHandler = envHandler <$> getEnv
    getConfiguration = envConfiguration <$> getEnv

--------------------------------------------------------------------------------
instance SessionLayer SessionM where
    getSession = ask
    getSessionID = sessionSessionID <$> ask
    getSessionState = sessionState <$> ask
    getChannelHub = sessionChannelHub <$> ask
    --getLocalBuffer = selectLocalBuffer . sessionBufferHub <$> ask
    --getGlobalBuffer = selectGlobalBuffer . sessionBufferHub <$> ask
    getListener = sessionListener <$> ask
    getTimeoutVar = sessionTimeoutVar <$> ask

--------------------------------------------------------------------------------
instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMSession { unStMSession :: StM (ReaderT Session ConnectionM) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMSession . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMSession
