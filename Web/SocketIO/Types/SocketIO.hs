--------------------------------------------------------------------------------
-- | Datatypes to be exposed to end user
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.SocketIO.Types.SocketIO where

--------------------------------------------------------------------------------
import 				Control.Applicative						(Applicative, (<$>), (<*>))
import 				Control.Concurrent.Chan.Lifted			(Chan, writeChan)
import              Control.Monad.Base
import              Control.Monad.Reader       
import              Control.Monad.Writer       
import              Data.Aeson                              

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String   

--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported.
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

instance Serializable Transport where
    serialize WebSocket = "websocket" 
    serialize XHRPolling = "xhr-polling" 
    serialize NoTransport = "unknown" 

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
type Payload = Text
type Buffer = Chan Emitter

type Listener = (Event, CallbackM ())
data Emitter  = Emitter Event [Payload] | NoEmitter deriving (Show, Eq)

instance Serializable Emitter where
    serialize = serialize . encode

--------------------------------------------------------------------------------
instance FromJSON Emitter where
    parseJSON (Object v) =  Emitter <$>
                            v .: "name" <*>
                            v .: "args"
    parseJSON _ = return NoEmitter

--------------------------------------------------------------------------------
instance ToJSON Emitter where
   toJSON (Emitter name args) = object ["name" .= name, "args" .= args]
   toJSON NoEmitter = object []


--------------------------------------------------------------------------------
data CallbackEnv = CallbackEnv
    {   callbackEnvPayload :: [Payload]
    ,   callbackEnvBufferHub :: BufferHub
    }

data BufferHub = BufferHub
    {   selectLocalBuffer :: Buffer
    ,   selectGlobalBuffer :: Buffer
    }
   
--------------------------------------------------------------------------------
-- | Capable of both sending and receiving events.
--
-- Use 'liftIO' if you wanna do some IO here.
newtype HandlerM a = HandlerM { runHandlerM :: (ReaderT BufferHub (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader BufferHub, MonadBase IO)

--------------------------------------------------------------------------------
-- | Capable of only sending events.
--
-- Use 'liftIO' if you wanna do some IO here.
newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT CallbackEnv IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Emitter], MonadReader CallbackEnv, MonadBase IO)


--------------------------------------------------------------------------------
-- | Sending events
class Publisher m where
    -- | Sends a message to the socket that starts it.
    --
    -- @
    -- `emit` \"launch\" [\"missile\", \"nuke\"] 
    -- @
    emit    :: Event                -- ^ event to trigger
            -> [Text]               -- ^ message to carry with
            -> m ()

    -- | Sends a message to everyone else except for the socket that starts it.
    --
    -- @
    -- `broadcast` \"hide\" [\"nukes coming!\"] 
    -- @
    broadcast   :: Event            -- ^ event to trigger
                -> [Text]           -- ^ message to carry with
                -> m ()

    -- | 

instance Publisher HandlerM where
    emit event reply = do
        channel <- selectLocalBuffer <$> ask
        writeChan channel (Emitter event reply)
    broadcast event reply = do
        channel <- selectGlobalBuffer <$> ask
        writeChan channel (Emitter event reply)

instance Publisher CallbackM where
    emit event reply = do
        channel <- CallbackM . lift $ selectLocalBuffer . callbackEnvBufferHub <$> ask
        writeChan channel (Emitter event reply)
    broadcast event reply = do
        channel <- CallbackM . lift $ selectGlobalBuffer . callbackEnvBufferHub <$> ask
        writeChan channel (Emitter event reply)

--------------------------------------------------------------------------------
-- | Receiving events.
class Subscriber m where
    -- @
    -- 'on' \"ping\" $ do
    --     'emit' \"pong\" []
    -- @
    on  :: Event            -- ^ event to listen to
        -> CallbackM ()     -- ^ callback
        -> m ()             

instance Subscriber HandlerM where
    on event callback = do
        HandlerM . lift . tell $ [(event, callback)]
