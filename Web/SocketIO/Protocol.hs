--------------------------------------------------------------------------------
-- | Socket.IO Protocol 1.0
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Protocol 
    (   parseMessage
    ,   parsePath
    ) where

import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>), (<*>))
import              Data.Aeson
import qualified    Data.ByteString                         as B
import              Data.Attoparsec.ByteString.Lazy
import              Data.Attoparsec.ByteString.Char8        (digit, decimal)
import              Prelude                                 hiding (take, takeWhile)

parseMessage :: ByteString -> Message
parseMessage input = case parseOnly messageParser input of
    Left err -> error $ show err
    Right msg -> msg
    
--------------------------------------------------------------------------------
-- | Message, not framed
messageParser :: Parser Message
messageParser = do
    n <- digit
    case n of
        '0' -> choice
            [   idParser >> endpointParser >>= return . MsgDisconnect
            ,                                  return $ MsgDisconnect NoEndpoint
            ]
        '1' -> choice
            [   idParser >> endpointParser >>= return . MsgConnect
            ,                                  return $ MsgConnect NoEndpoint 
            ]
        '2' -> return MsgHeartbeat
        '3' -> Msg          <$> idParser 
                            <*> endpointParser 
                            <*> dataParser
        '4' -> MsgJSON      <$> idParser 
                            <*> endpointParser 
                            <*> dataParser
        '5' -> MsgEvent     <$> idParser 
                            <*> endpointParser 
                            <*> eventParser
        '6' -> choice
            [   do  string ":::"
                    d <- decimal
                    string "+"
                    x <- takeWhile (const True)
                    
                    return $ MsgACK (ID d) (if B.null x then NoData else Data_ x)
                    
            ,   do  string ":::"
                    d <- decimal
                    return $ MsgACK (ID d) NoData
            ]
        '7' -> string ":" >> MsgError <$> endpointParser <*> dataParser
        '8' -> return MsgNoop
        _   -> return MsgNoop

idParser :: Parser ID
idParser = choice
    [   string ":" >> decimal >>= plus >>= return . IDPlus
    ,   string ":" >> decimal          >>= return . ID
    ,   string ":" >>                      return   NoID
    ]
    where   plus n = string "+" >> return n

endpointParser :: Parser Endpoint
endpointParser = do
    string ":"
    option NoEndpoint (takeWhile1 (/= 58) >>= return . Endpoint)

dataParser :: Parser Data_
dataParser = do
    string ":"
    option NoData (takeWhile1 (/= 58) >>= return . Data_)

eventParser :: Parser Event
eventParser = do
    string ":"
    t <- takeWhile (const True)
    case decode (serialize t) of
        Just e  -> return e
        Nothing -> return NoEvent

------------------------------------------------------------------------------               
-- | Parse given HTTP request
parsePath :: ByteString -> Path
parsePath p = case parseOnly pathParser p of
    Left _  -> Path XHRPolling (Just "Web.SocketIO.Protocol.parsePath_got_wrong")
    Right x -> x 

pathParser :: Parser Path
pathParser = do
    take 1 -- skip '&'
    pairs <- queryString

    --let transport = case parseOnly transportParser (fromJust (lookup "transport" pairs)) of
    --    Left _ -> NoTransport
    --    Just x -> x
    return (Path XHRPolling (lookup "sid" pairs))


    --option (WithoutSession namespace protocol) $ do
    --    transport <- transportParser
    --    string "/"
    --    sessionID <- takeTill (== 47)
    --    return $ WithSession namespace protocol transport sessionID


queryString :: Parser [(ByteString, ByteString)]
queryString = pair `sepBy1` word8 38
    where   field = takeTill (== 61)
            value = takeTill (\ c -> c == 38 || c == 0)
            pair = do
                f <- field
                take 1
                v <- value
                return (f, v)

transportParser :: Parser Transport
transportParser = choice
    [   string "websocket"      >> return WebSocket
    ,   string "polling"    >> return XHRPolling
    ,   string "unknown"        >> return NoTransport
    ,   skipWhile (/= 47)       >> return NoTransport
    ]