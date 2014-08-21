--------------------------------------------------------------------------------
-- | Engine.IO Protocol, please refer to <https://github.com/Automattic/engine.io-protocol>
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Web.SocketIO.Types.Protocol where

--------------------------------------------------------------------------------
import              Prelude                                     hiding (length)
import qualified    Data.ByteString                             as B
import              Data.ByteString                             (ByteString)
import              Data.Monoid                                 (mconcat)
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

data RequestView =  ConnectionOpen Transport (Maybe ByteString) Bool
                    deriving (Eq, Show)

viewRequest :: Request -> RequestView
viewRequest (Request "" t j Nothing b) = ConnectionOpen t j b


--------------------------------------------------------------------------------
-- | Encoding, see <https://github.com/Automattic/engine.io-protocol#encoding>

data Packet = Packet PacketType Data
                deriving (Eq, Show)


instance Serializable Packet where
    serialize (Packet t d)  =   serialize (toByteString $ [0] ++ len ++ [255])
                            <>  serialize t
                            <>  serialize d
        where   len = toDecimal (B.length d + 1)
                toDecimal n | n < 10 = [n]
                            | otherwise = toDecimal (n `div` 10) ++ [n `mod` 10]
                toByteString = B.pack . map toEnum

data PacketType = Open      -- 0
                | Close     -- 1
                | Ping      -- 2
                | Pong      -- 3
                | Message   -- 4
                | Upgrade   -- 5
                | Noop      -- 6
                deriving (Eq, Show)

instance Serializable PacketType where
    serialize Open = "0"
    serialize Close = "1"
    serialize Ping = "2"
    serialize Pong = "3"
    serialize Message = "4"
    serialize Upgrade = "5"
    serialize Noop = "6"

type Data = ByteString
data Payload = Payload SessionID [Packet] deriving (Eq, Show)

instance Serializable Payload where
    serialize (Payload _ packets) = mconcat $ map serialize packets


--------------------------------------------------------------------------------
-- | Now only xhr-polling is supported. <https://github.com/Automattic/engine.io-protocol#urls>
data Transport = WebSocket | XHRPolling | NoTransport deriving (Eq, Show)

instance Serializable Transport where
    serialize WebSocket = "websocket" 
    serialize XHRPolling = "polling" 
    serialize NoTransport = "unknown" 