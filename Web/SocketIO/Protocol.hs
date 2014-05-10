--------------------------------------------------------------------------------
-- | Socket.IO Protocol 1.0
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Protocol (parseFramedMessage, parsePath) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>), (<*>))
import              Data.Aeson
import qualified    Data.ByteString.Lazy                    as BL
--import              Text.Parsec
--import              Text.Parsec.ByteString.Lazy
import              Data.Attoparsec.ByteString.Lazy
import              Data.Attoparsec.ByteString.Char8        (digit, decimal)
import              Prelude                                 hiding (take)

--------------------------------------------------------------------------------
-- | Parse raw ByteString to Messages
parseFramedMessage :: BL.ByteString -> Framed Message
parseFramedMessage input = if isSingleton
    then Framed $ [parseMessage' input]
    else Framed $ map parseMessage' splitted
    where   splitted = split input
            parseMessage' x = case (eitherResult . parse messageParser) x of
                Left _  -> MsgNoop
                Right a -> a
            isSingleton = not (BL.null input) && BL.head input /= 239
--------------------------------------------------------------------------------
-- | Split raw ByteString with U+FFFD as delimiter
split :: BL.ByteString -> [BL.ByteString]
split str = map (BL.drop 2) . skipOddIndexed True . filter isDelimiter . BL.split 239 $ str
    where   isDelimiter x = not (BL.null x)
                             && (BL.head x == 191)
                             && (BL.head (BL.tail x) == 189)
            skipOddIndexed _ [] = []
            skipOddIndexed True (_:xs) = skipOddIndexed False xs
            skipOddIndexed False (x:xs) = x : skipOddIndexed True xs

----------------------------------------------------------------------------------
---- | Message, not framed
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
                    x <- takeTillEnd
                    return $ MsgACK (ID d) (Data x)
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
    t <- takeTillEnd
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

takeTillEnd :: Parser ByteString
takeTillEnd = takeTill (== 0)