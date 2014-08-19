--------------------------------------------------------------------------------
-- | Types for comsuming incoming data
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Request where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String
import              Web.SocketIO.Types.Base
import              Web.SocketIO.Types.Event

--------------------------------------------------------------------------------
import              Data.List                               (intersperse)
import              Data.Monoid                             (mconcat, mempty)
import qualified    Data.ByteString                         as B

--------------------------------------------------------------------------------
-- | Namespace
type Namespace = ByteString

--------------------------------------------------------------------------------
-- | Protocol running
type Protocol = ByteString

--------------------------------------------------------------------------------
-- | The URN part of a HTTP request.
-- Please refer to <https://github.com/LearnBoost/socket.io-spec#socketio-http-requests socket.io-spec#socketio-http-requests>
--data Request = Request Transport (Maybe SID)

type SID = ByteString

data Path   = Path Transport (Maybe SID)
            deriving (Eq, Show)

instance Serializable Path where
    serialize (Path t s) = "?transport=" <> serialize t
                                   <> "&sid=" <> serialize s

--------------------------------------------------------------------------------
-- | Incoming request
data Request    = Handshake
                | Disconnect SessionID
                | Connect SessionID 
                | Request SessionID Message
                deriving (Show)

--------------------------------------------------------------------------------
-- | This is how data are encoded by Socket.IO Protocol.
-- Please refer to <https://github.com/LearnBoost/socket.io-spec#messages socket.io-spec#messages>
data Message    = MsgHandshake SessionID Int Int [Transport]
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
instance Serializable Message where
    serialize (MsgHandshake s a b t)        = serialize (0 `B.cons` 8 `B.cons` 6 `B.cons` 255 `B.cons` "0{\"sid\":\"tDbNkKKtAahP1yFkAAAC\",\"upgrades\":[],\"pingInterval\":25000,\"pingTimeout\":60000}" :: ByteString)
        --where   transportType = fromString $ concat . intersperse "," . map serialize $ t :: ByteString
        --        a' = if a == 0 then mempty else serialize a
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

--------------------------------------------------------------------------------
-- | Message endpoint
data Endpoint   = Endpoint ByteString
                | NoEndpoint
                deriving (Show, Eq)

instance Serializable Endpoint where
    serialize (Endpoint s) = serialize s
    serialize NoEndpoint = ""

--------------------------------------------------------------------------------
-- | The message id is an incremental integer, required for ACKs (can be omitted). 
-- If the message id is followed by a +, the ACK is not handled by socket.io, but by the user instead.
data ID         = ID Int
                | IDPlus Int
                | NoID
                deriving (Show, Eq)

instance Serializable ID where
    serialize (ID i) = serialize i
    serialize (IDPlus i) = serialize i <> "+"
    serialize NoID = ""

--------------------------------------------------------------------------------
-- | Message data body
data Data       = Data ByteString
                | NoData
                deriving (Show, Eq)

instance Serializable Data where
    serialize (Data s) = serialize s
    serialize NoData = ""