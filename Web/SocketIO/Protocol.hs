--------------------------------------------------------------------------------
-- | Socket.IO Protocol 1.0
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Protocol 
    (   demultiplexMessage
    ,   parseFramedMessage
    ,   parsePath
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>), (<*>))
import              Data.Aeson
import qualified    Data.ByteString                         as B
import              Data.Conduit
import              Data.Conduit.Attoparsec                 (conduitParserEither)
import              Data.Attoparsec.ByteString.Lazy
import              Data.Attoparsec.ByteString.Char8        (digit, decimal)
import              Prelude                                 hiding (take, takeWhile)

--------------------------------------------------------------------------------
-- | Demultiplexing messages
demultiplexMessage :: Conduit ByteString IO Message
demultiplexMessage = do
    conduitParserEither framedMessageParser =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = mapM yield p

----------------------------------------------------------------------------------
---- | Using U+FFFD as delimiter
frameParser :: Parser a -> Parser a
frameParser parser = do
    string "ï¿½"
    len <- decimal
    string "ï¿½"
    x <- take len
    case parseOnly parser x of
        Left e  -> error e
        Right r -> return r

--------------------------------------------------------------------------------
-- | Message, framed with List
framedMessageParser :: Parser [Message]
framedMessageParser = choice [many1 (frameParser messageParser), many' messageParser]

--------------------------------------------------------------------------------
-- | Wrapped for testing
parseFramedMessage :: ByteString -> Framed Message
parseFramedMessage input = case parseOnly framedMessageParser input of
    Left e -> error e
    Right r -> Framed r

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
                    
                    return $ MsgACK (ID d) (if B.null x then NoData else Data x)
                    
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

dataParser :: Parser Data
dataParser = do
    string ":"
    option NoData (takeWhile1 (/= 58) >>= return . Data)

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
    Left _  -> WithoutSession "" ""
    Right x -> x 

pathParser :: Parser Path
pathParser = do
    string "/"
    namespace <- takeTill (== 47) -- 0x47: slash
    take 1  -- slip the second slash
    protocol <- takeTill (== 47)
    take 1  -- slip the third slash
    option (WithoutSession namespace protocol) $ do
        transport <- transportParser
        string "/"
        sessionID <- takeTill (== 47)
        return $ WithSession namespace protocol transport sessionID

transportParser :: Parser Transport
transportParser = choice
    [   string "websocket"      >> return WebSocket
    ,   string "xhr-polling"    >> return XHRPolling
    ,   string "unknown"        >> return NoTransport
    ,   skipWhile (/= 47)       >> return NoTransport
    ]