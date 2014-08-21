--------------------------------------------------------------------------------
-- | Engine.IO Protocol, please refer to <https://github.com/Automattic/engine.io-protocol>
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Protocol where

--------------------------------------------------------------------------------
import              Data.ByteString     
import              Web.SocketIO.Types.String

--------------------------------------------------------------------------------
-- | URLs, see <https://github.com/Automattic/engine.io-protocol#urls>

data Request = Request  {   reqBody :: ByteString
                        ,   reqTransport :: Transport
                        ,   reqJ :: Maybe ByteString
                        ,   reqSID :: Maybe SessionID
                        ,   reqB64 :: Bool
                        }
                        deriving (Eq, Show)

type SessionID = ByteString 

--------------------------------------------------------------------------------
-- view pattern of incoming requests

data RequestView =  Connect Transport (Maybe ByteString) Bool
                    deriving (Eq, Show)

viewRequest :: Request -> RequestView
viewRequest (Request "" t j Nothing b) = Connect t j b


--------------------------------------------------------------------------------
-- | Encoding, see <https://github.com/Automattic/engine.io-protocol#encoding>

data Packet = Packet PacketType Data
                deriving (Eq, Show)
data PacketType = Open     -- 0
                | Close     -- 1
                | Ping      -- 2
                | Pong      -- 3
                | Message   -- 4
                | Upgrade   -- 5
                | Noop      -- 6
                deriving (Eq, Show)

type Data = ByteString
type Payload = [Packet]


--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported. <https://github.com/Automattic/engine.io-protocol#urls>
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

instance Serializable Transport where
    serialize WebSocket = "websocket" 
    serialize XHRPolling = "polling" 
    serialize NoTransport = "unknown" 