--------------------------------------------------------------------------------
-- | Datatypes to be exposed to end user
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.SocketIO.Types.SocketIO where

--------------------------------------------------------------------------------
import 				Control.Applicative						(Applicative)
import 				Control.Concurrent.Chan.Lifted			(Chan, writeChan)
import              Control.Monad.Base
import              Control.Monad.Reader       
import              Control.Monad.Writer       
import              Control.Monad.Trans (liftIO)
import qualified    Data.Aeson                              as Aeson

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String

data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

data Configuration = Configuration
    {   transports :: [Transport]
    ,   logLevel :: Int
    ,   heartbeats :: Bool
    ,   closeTimeout :: Int
    ,   heartbeatTimeout :: Int
    ,   heartbeatInterval :: Int
    ,   pollingDuration :: Int
    } deriving Show

type Port = Int

type Event = Text
type Reply = [Text]
type Buffer = Chan Emitter
type Listener = (Event, CallbackM ())

data Emitter  = Emitter Event Reply | NoEmitter deriving (Show, Eq)

instance Aeson.ToJSON Emitter where
   toJSON (Emitter name args) = Aeson.object ["name" Aeson..= name, "args" Aeson..= args]
   toJSON NoEmitter = Aeson.object []
   
newtype SocketIOM a = SocketIOM { runSocketM :: (ReaderT Buffer (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader Buffer, MonadBase IO)

newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT Reply (ReaderT Buffer IO))) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Emitter], MonadReader Reply, MonadBase IO)


class Publisher m where
    emit :: Event -> Reply -> m ()

instance Publisher SocketIOM where
    emit event reply = do
        channel <- ask
        writeChan channel (Emitter event reply)

instance Publisher CallbackM where
    emit event reply = do
        channel <- CallbackM . lift . lift $ ask
        writeChan channel (Emitter event reply)

class Subscriber m where
    on :: Event -> CallbackM () -> m ()

instance Subscriber SocketIOM where
    on event callback = do
        SocketIOM . lift . tell $ [(event, callback)]
