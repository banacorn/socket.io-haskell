--------------------------------------------------------------------------------
-- | Engine.IO Protocol, please refer to <https://github.com/Automattic/engine.io-protocol>
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Protocol where

--------------------------------------------------------------------------------
import              Data.ByteString     
import              Web.SocketIO.Types.String

--------------------------------------------------------------------------------
-- | URLs, see <https://github.com/Automattic/engine.io-protocol#urls>

data Req = Req  {   reqTransport :: Transport
                ,   reqJ :: Int
                ,   reqSID :: SessionID
                ,   reqB64 :: Bool
                }

type SessionID = ByteString 

--------------------------------------------------------------------------------
-- | Encoding, see <https://github.com/Automattic/engine.io-protocol#encoding>

data Packet = Packet PacketType Data
data PacketType = Open      -- 0
                | Close     -- 1
                | Ping      -- 2
                | Pong      -- 3
                | Message   -- 4
                | Upgrade   -- 5
                | Noop      -- 6
data Data = ByteString

type Payload = [Packet]


--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported. <https://github.com/Automattic/engine.io-protocol#urls>
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

instance Serializable Transport where
    serialize WebSocket = "websocket" 
    serialize XHRPolling = "polling" 
    serialize NoTransport = "unknown" 