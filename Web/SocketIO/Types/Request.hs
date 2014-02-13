--------------------------------------------------------------------------------
-- | Types for comsuming incoming data

{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Request where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String

--------------------------------------------------------------------------------
import              Control.Applicative                     (Applicative, (<$>), (<*>))
import              Data.Aeson                              

--------------------------------------------------------------------------------
-- | Path of incoming request
type Namespace = ByteString
type Protocol = ByteString
type SessionID = ByteString 

data Path   = WithSession    Namespace Protocol Transport SessionID
            | WithoutSession Namespace Protocol
            deriving (Eq, Show)

instance Serializable Path where
    serialize (WithSession n p t s) = "/" <> serialize n 
                                   <> "/" <> serialize p 
                                   <> "/" <> serialize t
                                   <> "/" <> serialize s
                                   <> "/"
    serialize (WithoutSession n p)  = "/" <> serialize n
                                   <> "/" <> serialize p 
                                   <> "/"

--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported.
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

instance Serializable Transport where
    serialize WebSocket = "websocket" 
    serialize XHRPolling = "xhr-polling" 
    serialize NoTransport = "unknown" 

--------------------------------------------------------------------------------
-- | Event
type EventName = Text
type Payload = Text
data Event = Event EventName [Payload] | NoEvent deriving (Show, Eq)
type Package = (SessionID, Event)

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
-- | Incoming request
data Request    = Handshake
                | Disconnect SessionID
                | Connect SessionID 
                | Emit SessionID Event
                deriving (Show)

--------------------------------------------------------------------------------
-- | This is how data are encoded by Socket.IO Protocol
data Message    = MsgRaw ByteString
                | MsgDisconnect Endpoint
                | MsgConnect Endpoint
                | MsgHeartbeat
                | Msg ID Endpoint Data
                | MsgJSON ID Endpoint Data
                | MsgEvent ID Endpoint Event
                | MsgACK ID Data
                | MsgError Endpoint Data
                | MsgNoop
                deriving (Show, Eq)

data Endpoint   = Endpoint ByteString
                | NoEndpoint
                deriving (Show, Eq)
data ID         = ID Int
                | IDPlus Int
                | NoID
                deriving (Show, Eq)
data Data       = Data ByteString
                | NoData
                deriving (Show, Eq)


--------------------------------------------------------------------------------
instance Serializable Endpoint where
    serialize (Endpoint s) = serialize s
    serialize NoEndpoint = ""

instance Serializable ID where
    serialize (ID i) = serialize $ show i
    serialize (IDPlus i) = serialize $ show i <> "+"
    serialize NoID = ""

instance Serializable Data where
    serialize (Data s) = serialize s
    serialize NoData = ""

instance Serializable Message where
    serialize (MsgRaw s)                    = serialize s
    serialize (MsgDisconnect NoEndpoint)    = "0"
    serialize (MsgDisconnect e)             = "0::" <> serialize e
    serialize (MsgConnect e)                = "1::" <> serialize e
    serialize MsgHeartbeat                  = "2::"
    serialize (Msg i e d)                   = "3:" <> serialize i <>
                                                       ":" <> serialize e <>
                                              ":" <> serialize d
    serialize (MsgJSON i e d)               = "4:" <> serialize i <>
                                              ":" <> serialize e <>
                                              ":" <> serialize d
    serialize (MsgEvent i e d)              = "5:" <> serialize i <>
                                              ":" <> serialize e <>
                                              ":" <> serialize d
    serialize (MsgACK i d)                  = "6:::" <> serialize i <> 
                                              "+" <> serialize d
    serialize (MsgError e d)                = "7::" <> serialize e <> 
                                              ":" <> serialize d
    serialize MsgNoop                       = "8:::"
