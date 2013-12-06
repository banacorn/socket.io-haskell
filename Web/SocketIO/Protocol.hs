--------------------------------------------------------------------------------
-- | Socket.IO Protocol 1.0
{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Protocol (parseMessage, parsePath) where

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>), (<*>))
import              Data.Aeson
import              Data.ByteString                         (ByteString)
import qualified    Data.ByteString.Lazy                    as BL
import              Text.ParserCombinators.Parsec
--------------------------------------------------------------------------------
import Web.SocketIO.Types


--------------------------------------------------------------------------------
parseMessage :: BL.ByteString -> Message
parseMessage text = case parse parseMessage' "" string of
    Left _  -> MsgNoop
    Right x -> x
    where   string = fromLazyByteString text

--------------------------------------------------------------------------------
parseMessage' :: Parser Message
parseMessage' = do
    n <- digit
    case n of
        '0' ->  (parseEndpoint >>= return . MsgDisconnect)
            <|> (                  return $ MsgDisconnect NoEndpoint)
        '1' ->  (parseEndpoint >>= return . MsgConnect)
            <|> (                  return $ MsgConnect NoEndpoint)
        '2' ->  return MsgHeartbeat
        '3' ->  parseRegularMessage Msg
        '4' ->  parseRegularMessage MsgJSON
        '5' ->  MsgEvent    <$> parseID 
                            <*> parseEndpoint 
                            <*> parseEmitter
        '6' ->  try (do 
                string ":::"
                n <- read <$> number
                char '+'
                d <- fromString <$> text
                return $ MsgACK (ID n) (Data d)
            ) <|> (do
                string ":::"
                n <- read <$> number
                return $ MsgACK (ID n) NoData
            )
        '7' -> colon >> MsgError <$> parseEndpoint <*> parseData
        '8' ->  return $ MsgNoop
        _   ->  return $ MsgNoop
    where   parseRegularMessage ctr = ctr <$> parseID 
                                          <*> parseEndpoint 
                                          <*> parseData

--------------------------------------------------------------------------------
endpoint = many1 $ satisfy (/= ':')
text = many1 anyChar
number = many1 digit
colon = char ':'

--------------------------------------------------------------------------------
parseID :: Parser ID
parseID  =  try (colon >> number >>= plus >>= return . IDPlus . read)
        <|> try (colon >> number          >>= return . ID . read)
        <|>     (colon >>                     return   NoID)
        where   plus n = char '+' >> return n 

--------------------------------------------------------------------------------
parseEndpoint :: Parser Endpoint
parseEndpoint    =  try (colon >> endpoint >>= return . Endpoint)
                <|>     (colon >>              return   NoEndpoint)

--------------------------------------------------------------------------------
parseData :: Parser Data
parseData    =  try (colon >> text >>= return . Data . fromString)
            <|>     (colon >>          return   NoData)

--------------------------------------------------------------------------------
parseEmitter :: Parser Emitter
parseEmitter =  try (do
                colon
                t <- text
                case decode (fromString t) of
                    Just e -> return e
                    Nothing -> return NoEmitter
            )
            <|>     (colon >>          return   NoEmitter)


--------------------------------------------------------------------------------

textWithoutSlash :: Parser String
textWithoutSlash = many1 $ satisfy (/= '/')

slash :: Parser Char
slash = char '/'


-------------------------------------------------------------------------------
parseTransport :: Parser Transport
parseTransport = try (string "websocket" >> return WebSocket) 
        <|> (string "xhr-polling" >> return XHRPolling)
        <|> return NoTransport

--------------------------------------------------------------------------------               
parsePath :: ByteString -> Path
parsePath path = case parse parsePath' "" string of
    Left _  -> WithoutSession "" ""
    Right x -> x 
    where   string = fromByteString path

--------------------------------------------------------------------------------
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
