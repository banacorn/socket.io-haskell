{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Web.SocketIO.Type where

import Web.SocketIO.Util

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
import qualified Data.Aeson as Aeson
import Data.IORef.Lifted
import Data.Monoid ((<>))

type Text = TL.Text
type Event = Text
type Reply = [Text]
type SessionID = Text 

type Port = Int

type Listener = (Event, CallbackM ())
data Emitter  = Emitter Event Reply | NoEmitter deriving (Show, Eq)

instance Aeson.ToJSON Emitter where
   toJSON (Emitter name args) = Aeson.object ["name" Aeson..= name, "args" Aeson..= args]





-- options

data Configuration = Configuration {
    transports :: [Transport]
} deriving Show

data Transport = WebSocket | XHRPolling | NoTransport deriving Show

instance Msg Transport where
    toMessage WebSocket = "websocket"
    toMessage XHRPolling = "xhr-polling"










type Table = H.HashMap SessionID Session 
data Status = Connecting | Connected | Disconnecting deriving Show
type Buffer = Chan Emitter

data Request    = RHandshake
                | RDisconnect SessionID
                | RConnect SessionID 
                | REmit SessionID Emitter
                deriving (Show)

data SessionState   = Syn
                    | Ack
                    | Polling
                    | Emit Emitter
                    | Disconnect
                    | Error

data Env = Env { 
    sessionTable :: IORef Table, 
    handler :: SocketM (), 
    configuration :: Configuration 
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


data Message    = MsgDisconnect Endpoint
                | MsgConnect Endpoint
                | MsgHeartbeat
                | Msg ID Endpoint Data
                | MsgJSON ID Endpoint Data
                | MsgEvent ID Endpoint Emitter
                | MsgACK ID Data
                | MsgError Endpoint Data
                | MsgNoop
                deriving (Show, Eq)

data Endpoint   = Endpoint String
                | NoEndpoint
                deriving (Show, Eq)
data ID         = ID Int
                | IDPlus Int
                | NoID
                deriving (Show, Eq)
data Data       = Data Text
                | NoData
                deriving (Show, Eq)



class Msg m where
    toMessage :: m -> Text

instance Msg Endpoint where
    toMessage (Endpoint s) = TL.pack s
    toMessage NoEndpoint = ""

instance Msg ID where
    toMessage (ID i) = TL.pack $ show i
    toMessage (IDPlus i) = TL.pack $ show i ++ "+"
    toMessage NoID = ""

instance Msg Data where
    toMessage (Data s) = s
    toMessage NoData = ""

instance Msg Emitter where
    toMessage = fromLazyByteString . Aeson.encode 

instance Msg Message where
    toMessage (MsgDisconnect NoEndpoint)    = "0"
    toMessage (MsgDisconnect e)             = "0::" <> toMessage e
    toMessage (MsgConnect e)                = "1::" <> toMessage e
    toMessage MsgHeartbeat                  = undefined
    toMessage (Msg i e d)                   = "3:" <> toMessage i <>
                                              ":" <> toMessage e <>
                                              ":" <> toMessage d
    toMessage (MsgJSON i e d)               = "4:" <> toMessage i <>
                                              ":" <> toMessage e <>
                                              ":" <> toMessage d
    toMessage (MsgEvent i e d)              = "5:" <> toMessage i <>
                                              ":" <> toMessage e <>
                                              ":" <> toMessage d
    toMessage (MsgACK i d)                  = "6:::" <> toMessage i <> 
                                              "+" <> toMessage d
    toMessage (MsgError e d)                = "7::" <> toMessage e <> 
                                              ":" <> toMessage d
    toMessage MsgNoop                       = "8:::"