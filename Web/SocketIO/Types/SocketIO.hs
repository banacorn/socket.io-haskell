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

--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported.
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

--------------------------------------------------------------------------------
data Configuration = Configuration
    {   transports :: [Transport]
    ,   logLevel :: Int
    ,   heartbeats :: Bool
    ,   closeTimeout :: Int
    ,   heartbeatTimeout :: Int
    ,   heartbeatInterval :: Int
    ,   pollingDuration :: Int
    } deriving Show

--------------------------------------------------------------------------------
type Port = Int

--------------------------------------------------------------------------------
type Event = Text
type Buffer = Chan Emitter

type Listener = (Event, CallbackM ())
data Emitter  = Emitter Event [Text] | NoEmitter deriving (Show, Eq)

instance Aeson.ToJSON Emitter where
   toJSON (Emitter name args) = Aeson.object ["name" Aeson..= name, "args" Aeson..= args]
   toJSON NoEmitter = Aeson.object []
   
-- | The outermost layer of context, capable of both subscribing and publishing and doing IO.
newtype HandlerM a = HandlerM { runHandlerM :: (ReaderT Buffer (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader Buffer, MonadBase IO)

-- | Capable of only publishing and doing IO.
newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT [Text] (ReaderT Buffer IO))) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Emitter], MonadReader [Text], MonadBase IO)

class Publisher m where
    emit :: Event -> [Text] -> m ()

instance Publisher HandlerM where
    emit event reply = do
        channel <- ask
        writeChan channel (Emitter event reply)

instance Publisher CallbackM where
    emit event reply = do
        channel <- CallbackM . lift . lift $ ask
        writeChan channel (Emitter event reply)

class Subscriber m where
    on :: Event -> CallbackM () -> m ()

instance Subscriber HandlerM where
    on event callback = do
        HandlerM . lift . tell $ [(event, callback)]
