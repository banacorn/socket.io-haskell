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
type EventName = Text
type Payload = Text

type Listener = (EventName, CallbackM ())
data Event  = Event EventName [Payload] | NoEvent deriving (Show, Eq)

instance Serializable Event where
    serialize = serialize . encode

--------------------------------------------------------------------------------
instance FromJSON Event where
    parseJSON (Object v) =  Event <$>
                            v .: "name" <*>
                            v .: "args"
    parseJSON _ = return NoEvent

--------------------------------------------------------------------------------
instance ToJSON Event where
   toJSON (Event name args) = object ["name" .= name, "args" .= args]
   toJSON NoEvent = object []


--------------------------------------------------------------------------------
data CallbackEnv = CallbackEnv
    {   callbackEnvPayload :: [Payload]
    ,   callbackEnvChannelHub :: ChannelHub
    }

data ChannelHub = ChannelHub
    {   channelHubLocal :: Chan Event
    ,   channelHubGlobal :: Chan Event
    ,   channelHubOutput :: Chan Event
    ,   channelHubLog :: Chan ByteString
    }
   
--------------------------------------------------------------------------------
-- | Capable of both sending and receiving events.
--
-- Use 'liftIO' if you wanna do some IO here.
newtype HandlerM a = HandlerM { runHandlerM :: (ReaderT ChannelHub (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader ChannelHub, MonadBase IO)

--------------------------------------------------------------------------------
-- | Capable of only sending events.
--
-- Use 'liftIO' if you wanna do some IO here.
newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Event] (ReaderT CallbackEnv IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Event], MonadReader CallbackEnv, MonadBase IO)


--------------------------------------------------------------------------------
-- | Sending events
class Publisher m where
    -- | Sends a message to the socket that starts it.
    --
    -- @
    -- `emit` \"launch\" [\"missile\", \"nuke\"] 
    -- @
    emit    :: EventName            -- ^ name of event to trigger
            -> [Text]               -- ^ message to carry with
            -> m ()

    -- | Sends a message to everyone else except for the socket that starts it.
    --
    -- @
    -- `broadcast` \"hide\" [\"nukes coming!\"] 
    -- @
    broadcast   :: EventName        -- ^ name of event to trigger
                -> [Text]           -- ^ message to carry with
                -> m ()

    -- | 

instance Publisher HandlerM where
    emit event reply = do
        channel <- channelHubLocal <$> ask
        writeChan channel (Event event reply)
    broadcast event reply = do
        channel <- channelHubGlobal <$> ask
        writeChan channel (Event event reply)

instance Publisher CallbackM where
    emit event reply = do
        channel <- CallbackM . lift $ channelHubLocal . callbackEnvChannelHub <$> ask
        writeChan channel (Event event reply)
    broadcast event reply = do
        channel <- CallbackM . lift $ channelHubGlobal . callbackEnvChannelHub <$> ask
        writeChan channel (Event event reply)

--------------------------------------------------------------------------------
-- | Receiving events.
class Subscriber m where
    -- @
    -- 'on' \"ping\" $ do
    --     'emit' \"pong\" []
    -- @
    on  :: EventName        -- ^ name of event to listen to
        -> CallbackM ()     -- ^ callback
        -> m ()             

instance Subscriber HandlerM where
    on event callback = do
        HandlerM . lift . tell $ [(event, callback)]
