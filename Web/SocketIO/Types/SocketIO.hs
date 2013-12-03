--------------------------------------------------------------------------------
-- | Datatypes to be exposed to end user
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.SocketIO.Types.SocketIO where

--------------------------------------------------------------------------------
import 				Control.Applicative						(Applicative, (<$>))
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

data BufferHub = BufferHub
    {   selectLocalBuffer :: Buffer
    ,   selectGlobalBuffer :: Buffer
    }
   
-- | The outermost layer of context, capable of both subscribing and publishing and doing IO.
newtype HandlerM a = HandlerM { runHandlerM :: (ReaderT BufferHub (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader BufferHub, MonadBase IO)

-- | Capable of only publishing and doing IO.
newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT [Text] (ReaderT BufferHub IO))) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Emitter], MonadReader [Text], MonadBase IO)

class Publisher m where
    emit :: Event -> [Text] -> m ()
    broadcast :: Event -> [Text] -> m ()

instance Publisher HandlerM where
    emit event reply = do
        channel <- selectLocalBuffer <$> ask
        writeChan channel (Emitter event reply)
    broadcast event reply = do
        error "yooo"

instance Publisher CallbackM where
    emit event reply = do
        channel <- CallbackM . lift . lift $ selectLocalBuffer <$> ask
        writeChan channel (Emitter event reply)
    broadcast event reply = do
        channel <- CallbackM . lift . lift $ selectGlobalBuffer <$> ask
        writeChan channel (Emitter event reply)
        --error "CallbackM "

class Subscriber m where
    on :: Event -> CallbackM () -> m ()

instance Subscriber HandlerM where
    on event callback = do
        HandlerM . lift . tell $ [(event, callback)]