{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Web.SocketIO.Types
    (   module Web.SocketIO.Types.Log
    ,   module Web.SocketIO.Types.Request
    ,   module Web.SocketIO.Types.Event
    ,   module Web.SocketIO.Types.SocketIO
    ,   module Web.SocketIO.Types.String
    ,   ConnectionM(..)
    ,   SessionM(..)
    ,   SocketM(..)
    ,   CallbackM(..)
    ,   ConnectionLayer(..)
    ,   SessionLayer(..)
    ,   Env(..)
    ,   Session(..)
    ,   SessionState(..)
    ,   Buffer
    ,   Listener
    ,   Status(..)
    ,   Request(..)
    ,   Table
    
    ) where

import Web.SocketIO.Types.Request
import Web.SocketIO.Types.Log
import Web.SocketIO.Types.String
import Web.SocketIO.Types.Event
import Web.SocketIO.Types.SocketIO

import qualified Network.Wai as Wai

import Control.Monad.Reader       
import Control.Monad.Writer       
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Applicative

import qualified Data.HashMap.Strict as H
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef.Lifted



type Listener = (Event, CallbackM ())
 
type Table = H.HashMap SessionID Session 
data Status = Connecting | Connected | Disconnecting deriving Show
type Buffer = Chan Emitter

data Request    = Handshake
                | Disconnect SessionID
                | Connect SessionID 
                | Emit SessionID Emitter
                deriving (Show)

data SessionState   = SessionSyn
                    | SessionAck
                    | SessionSocket
                    | SessionPolling
                    | SessionEmit Emitter
                    | SessionDisconnect
                    | SessionError

data Env = Env { 
    sessionTable :: IORef Table, 
    handler :: SocketM (), 
    configuration :: Configuration,
    stdout :: Chan String
}

class ConnectionLayer m where
    getEnv :: m Env
    getSessionTable :: m (IORef Table)
    getHandler :: m (SocketM ())
    getConfiguration :: m Configuration


class SessionLayer m where
    getSession :: m Session
    getSessionID :: m SessionID
    getStatus :: m Status
    getBuffer :: m Buffer
    getListener :: m [Listener]
    getTimeoutVar :: m (MVar ())



newtype ConnectionM a = ConnectionM { runConnectionM :: ReaderT Env IO a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Env, MonadBase IO)

instance ConnectionLayer ConnectionM where
    getEnv = ask
    getSessionTable = sessionTable <$> ask
    getHandler = handler <$> ask
    getConfiguration = configuration <$> ask

instance (MonadBaseControl IO) ConnectionM where
    newtype StM ConnectionM a = StMConnection { unStMConnection :: StM (ReaderT Env IO) a }
    liftBaseWith f = ConnectionM (liftBaseWith (\run -> f (liftM StMConnection . run . runConnectionM)))
    restoreM = ConnectionM . restoreM . unStMConnection

data Session = Session { 
    sessionID :: SessionID, 
    status :: Status, 
    buffer :: Buffer, 
    listener :: [Listener],
    timeoutVar :: MVar ()
} | NoSession



newtype SessionM a = SessionM { runSessionM :: (ReaderT Session ConnectionM) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Session, MonadBase IO)


instance ConnectionLayer SessionM where
    getEnv = SessionM (lift ask)
    getSessionTable = sessionTable <$> getEnv
    getHandler = handler <$> getEnv
    getConfiguration = configuration <$> getEnv

instance SessionLayer SessionM where
    getSession = ask
    getSessionID = sessionID <$> ask
    getStatus = status <$> ask
    getBuffer = buffer <$> ask
    getListener = listener <$> ask
    getTimeoutVar = timeoutVar <$> ask

instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMSession { unStMSession :: StM (ReaderT Session ConnectionM) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMSession . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMSession

newtype SocketM a = SocketM { runSocketM :: (ReaderT Buffer (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader Buffer, MonadBase IO)

newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT Reply (ReaderT Buffer IO))) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Emitter], MonadReader Reply, MonadBase IO)
