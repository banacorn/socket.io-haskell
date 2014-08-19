--------------------------------------------------------------------------------
-- | Engine.IO Protocol, please refer to https://github.com/Automattic/engine.io-protocol
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Protocol where

import              Data.ByteString     

--------------------------------------------------------------------------------
-- | Encoding, https://github.com/Automattic/engine.io-protocol#encoding

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
