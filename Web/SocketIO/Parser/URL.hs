--------------------------------------------------------------------------------
-- | URL Parsing

{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Parser.URL (requestP) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.Protocol


--------------------------------------------------------------------------------
import              Prelude                         hiding (take, takeWhile)
import              Data.ByteString                 hiding (take, takeWhile)
import              Data.Attoparsec.ByteString


--------------------------------------------------------------------------------
-- | URL query string
queryStringP :: Parser [(ByteString, ByteString)]
queryStringP = pair `sepBy1` word8 38
    where   field = takeTill (== 61)
            value = takeTill (\ c -> c == 38 || c == 0)
            pair = do
                f <- field
                take 1
                v <- value
                return (f, v)

--------------------------------------------------------------------------------
-- | Parse query string and build a Req
requestP :: Parser Req
requestP = do

    pairs <- queryStringP

    let transport = case lookup "transport" pairs of
            Just "polling" -> XHRPolling
            Just "websocket" -> WebSocket
            _         -> error "failed to parse transport field in query string"

    let b64 = case lookup "b64" pairs of
            Just "1" -> True
            _        -> False

    return Req  { reqTransport  = transport
                , reqJ          = lookup "j" pairs
                , reqSID        = lookup "sid" pairs
                , reqB64        = b64
                , reqBody       = ""
                }
