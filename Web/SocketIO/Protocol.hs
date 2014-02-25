--------------------------------------------------------------------------------
-- | Socket.IO Protocol 1.0
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Protocol (parseMessage, parsePath) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>), (<*>))
import              Data.Aeson
import qualified    Data.ByteString.Lazy                    as BL
import              Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------
-- | Parse raw ByteString to Message
parseMessage :: BL.ByteString -> [Message]
parseMessage raw = case parse parseMessage' "" str of
    Left _  -> [MsgNoop]
    Right x -> [x]
    where   str = fromLazyByteString raw

--------------------------------------------------------------------------------
-- | Parsec version, without wrapper
parseMessage' :: Parser Message
parseMessage' = do
    n <- digit
    case n of
        '0' ->  (parseID >> parseEndpoint >>= return . MsgDisconnect)
            <|> (                  return $ MsgDisconnect NoEndpoint)
        '1' ->  (parseID >> parseEndpoint >>= return . MsgConnect)
            <|> (                  return $ MsgConnect NoEndpoint)
        '2' ->  return MsgHeartbeat
        '3' ->  parseRegularMessage Msg
        '4' ->  parseRegularMessage MsgJSON
        '5' ->  MsgEvent    <$> parseID 
                            <*> parseEndpoint 
                            <*> parseEvent
        '6' ->  try (do 
                string ":::"
                n' <- read <$> number
                char '+'
                d <- fromString <$> text
                return $ MsgACK (ID n') (Data d)
            ) <|> (do
                string ":::"
                n' <- read <$> number
                return $ MsgACK (ID n') NoData
            )
        '7' -> colon >> MsgError <$> parseEndpoint <*> parseData
        '8' ->  return $ MsgNoop
        _   ->  return $ MsgNoop
    where   parseRegularMessage ctr = ctr <$> parseID 
                                          <*> parseEndpoint 
                                          <*> parseData

--------------------------------------------------------------------------------
endpoint :: Parser String
endpoint = many1 $ satisfy (/= ':')

--------------------------------------------------------------------------------
-- | Parser combinator for non-empty text
text :: Parser String
text = many1 anyChar

--------------------------------------------------------------------------------
number :: Parser String
number = many1 digit

--------------------------------------------------------------------------------
colon :: Parser Char
colon = char ':'

--------------------------------------------------------------------------------
parseID :: Parser ID
parseID  =  try (colon >> number >>= plus >>= return . IDPlus . read)
        <|> try (colon >> number          >>= return . ID . read)
        <|>     (colon >>                     return   NoID)
        where   plus n = char '+' >> return n 

--------------------------------------------------------------------------------
parseEndpoint :: Parser Endpoint
parseEndpoint    =  try (colon >> fromString <$> endpoint >>= return . Endpoint)
                <|>     (colon >>              return   NoEndpoint)

--------------------------------------------------------------------------------
parseData :: Parser Data
parseData    =  try (colon >> text >>= return . Data . fromString)
            <|>     (colon >>          return   NoData)

--------------------------------------------------------------------------------
parseEvent :: Parser Event
parseEvent = try (do
                colon
                t <- text
                case decode (fromString t) of
                    Just e -> return e
                    Nothing -> return NoEvent
            )
            <|>     (colon >>          return   NoEvent)


--------------------------------------------------------------------------------
-- | Slashes as delimiters
textWithoutSlash :: Parser String
textWithoutSlash = many1 $ satisfy (/= '/')

slash :: Parser Char
slash = char '/'

-------------------------------------------------------------------------------
parseTransport :: Parser Transport
parseTransport = try (string "websocket" >> return WebSocket) 
        <|> (string "xhr-polling" >> return XHRPolling)
        <|> (string "unknown" >> return NoTransport)
        <|> return NoTransport

--------------------------------------------------------------------------------               
-- | With wrapper
parsePath :: ByteString -> Path
parsePath path = case parse parsePath' "" (fromByteString path) of
    Left _  -> WithoutSession "" ""
    Right x -> x 

--------------------------------------------------------------------------------
-- | Raw Parsec combinator, without wrapper
parsePath' :: Parser Path
parsePath' = do
    slash
    namespace <- fromString <$> textWithoutSlash
    slash
    protocol <- fromString <$> textWithoutSlash
    slash
    try (do
        transport <- parseTransport
        slash
        sessionID <- fromString <$> textWithoutSlash
        return $ WithSession namespace protocol transport sessionID
        ) <|> (return $ WithoutSession namespace protocol)