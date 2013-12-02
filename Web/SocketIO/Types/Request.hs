--------------------------------------------------------------------------------
-- | Types for comsuming incoming data

{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Request where

--------------------------------------------------------------------------------
import qualified    Data.Aeson                              as Aeson
import qualified    Data.Text.Lazy                          as TL

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String
import              Web.SocketIO.Types.Event


--------------------------------------------------------------------------------
-- | Path of incoming request
type Namespace = Text
type Protocol = Text
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)
type SessionID = Text 

data Path   = WithSession    Namespace Protocol Transport SessionID
            | WithoutSession Namespace Protocol
            deriving (Eq, Show)



--------------------------------------------------------------------------------
-- | This is how data are encoded by Socket.IO Protocol
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


--------------------------------------------------------------------------------
-- | A typeclass for converting everything to Text for output
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
    toMessage MsgHeartbeat                  = "2::"
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

instance Msg Transport where
    toMessage WebSocket = "websocket"
    toMessage XHRPolling = "xhr-polling"
