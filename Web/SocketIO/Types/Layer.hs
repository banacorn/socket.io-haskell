{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Web.SocketIO.Types.Layer
    (   ConnectionM(..)
    ,   SessionM(..)
    ,   ConnectionLayer(..)
    ,   SessionLayer(..)
    ) where

----------------------------------------------------------------------------------
import              Web.SocketIO.Types.Base

----------------------------------------------------------------------------------
import              Control.Applicative
import              Control.Concurrent.MVar.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Trans.Control
import              Control.Monad.Base
import              Data.IORef.Lifted

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
    getListener = sessionListener <$> ask
    getTimeoutVar = sessionTimeoutVar <$> ask

--------------------------------------------------------------------------------
instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMSession { unStMSession :: StM (ReaderT Session ConnectionM) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMSession . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMSession