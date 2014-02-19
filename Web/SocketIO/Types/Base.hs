{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Base where
--module Web.SocketIO.Types.Base
--    (   Env(..)
--    ,   Session(..)
--    ,   SessionID
--    ,   SessionState(..)
--    ,   SessionAction(..)
--    ,   Table
--    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.Log
import              Web.SocketIO.Types.String

--------------------------------------------------------------------------------
import              Control.Applicative
import              Control.Concurrent.MVar.Lifted
import              Control.Concurrent.Chan.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Writer
import              Control.Monad.Base

import              Data.Aeson                              
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
type SessionID = ByteString 
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
data ChannelHub = ChannelHub
    {   channelHubLocal :: Chan Package
    ,   channelHubGlobal :: Chan Package
    ,   channelHubOutput :: Chan Package
    ,   channelHubLog :: Chan ByteString
    }

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
    
type Port = Int
type Listener = (EventName, CallbackM ())

--------------------------------------------------------------------------------
data HandlerEnv = HandlerEnv
    {   handlerEnvChannelHub :: ChannelHub
    ,   handlerEnvSessionID :: SessionID
    }

--------------------------------------------------------------------------------
data CallbackEnv = CallbackEnv
    {   callbackEnvPayload :: [Payload]
    ,   callbackEnvChannelHub :: ChannelHub
    ,   callbackEnvSessionID :: SessionID
    }

   
--------------------------------------------------------------------------------
-- | Capable of both sending and receiving events.
--
-- Use 'liftIO' if you wanna do some IO here.
newtype HandlerM a = HandlerM { runHandlerM :: (ReaderT HandlerEnv (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader HandlerEnv, MonadBase IO)

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
        channel <- channelHubLocal . handlerEnvChannelHub <$> ask
        writeChan channel (Private, Event event reply)
    broadcast event reply = do
        channel <- channelHubGlobal . handlerEnvChannelHub <$> ask
        sessionID <- handlerEnvSessionID <$> ask
        writeChan channel (Broadcast sessionID, Event event reply)

instance Publisher CallbackM where
    emit event reply = do
        channel <- CallbackM . lift $ channelHubLocal . callbackEnvChannelHub <$> ask
        writeChan channel (Private, Event event reply)
    broadcast event reply = do
        channel <- CallbackM . lift $ channelHubGlobal . callbackEnvChannelHub <$> ask
        sessionID <- CallbackM . lift $ callbackEnvSessionID <$> ask
        writeChan channel (Broadcast sessionID, Event event reply)

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

--------------------------------------------------------------------------------
-- | Event
type EventName = Text
type Payload = Text
data Event = Event EventName [Payload] | NoEvent deriving (Show, Eq)
data EventType = Private | Broadcast SessionID deriving (Show, Eq)
type Package = (EventType, Event)

instance Serializable Event where
    serialize = serialize . encode

instance FromJSON Event where
    parseJSON (Object v) =  Event <$>
                            v .: "name" <*>
                            v .: "args"
    parseJSON _ = return NoEvent

instance ToJSON Event where
   toJSON (Event name args) = object ["name" .= name, "args" .= args]
   toJSON NoEvent = object []
   
--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported.
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

instance Serializable Transport where
    serialize WebSocket = "websocket" 
    serialize XHRPolling = "xhr-polling" 
    serialize NoTransport = "unknown" 