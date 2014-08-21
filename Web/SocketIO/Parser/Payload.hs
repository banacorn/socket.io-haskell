--------------------------------------------------------------------------------
-- | Payload Parsing

{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Parser.Payload (payloadP) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.Protocol


--------------------------------------------------------------------------------
import              Prelude                         hiding (take, takeWhile, map)
import              Data.ByteString                 hiding (take, takeWhile)
import              Data.Attoparsec.ByteString


--------------------------------------------------------------------------------
-- | 
packetTypeP :: Parser packetTypeP
packetTypeP = do
    t <- take 1
    case t of
        "0" -> return Open
        "1" -> return Close
        "2" -> return Ping
        "3" -> return Pong
        "4" -> return Message
        "5" -> return Upgrade
        "6" -> return Noop

packetP :: Int -> Parser Packet
packetP len = do
    packetType <- packetTypeP
    content <- take len
    return (Packet packetType content)

encodedPacketP :: Parser Packet
encodedPacketP = do
    take 1
    contentLenRaw <- takeTill 255
    take 1
    let contentLen = foldl (\ acc c -> ord c + acc * 10) 0 contentLenRaw
    packetP contentLen

payloadP :: Parser Payload
payloadP = many encodedPacketP