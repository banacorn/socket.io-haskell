--------------------------------------------------------------------------------
-- | Socket.IO Protocol 1.0
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Protocol (parseFramedMessage, parsePath) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>), (<*>))
import              Data.Aeson
import qualified    Data.ByteString.Lazy                    as B
import qualified    Data.ByteString.Lazy                    as BL
--import              Text.Parsec
--import              Text.Parsec.ByteString.Lazy
import              Data.Attoparsec.ByteString.Lazy
import              Prelude                                 hiding (take)


parseMessage = undefined
--parsePath = undefined
--------------------------------------------------------------------------------
-- | Parse raw ByteString to Messages
parseFramedMessage :: BL.ByteString -> Framed Message
parseFramedMessage input = if isSingleton
    then Framed $ [parseMessage' input]
    else Framed $ map parseMessage' splitted
    where   splitted = split input
            parseMessage' x = case (eitherResult . parse parseMessage) x of
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
--parseMessage :: Parser Message
--parseMessage = do
--    n <- digit
--    case n of
--        '0' ->  (parseID >> parseEndpoint >>= return . MsgDisconnect)
--            <|> (                  return $ MsgDisconnect NoEndpoint)
--        '1' ->  (parseID >> parseEndpoint >>= return . MsgConnect)
--            <|> (                  return $ MsgConnect NoEndpoint)
--        '2' ->  return MsgHeartbeat
--        '3' ->  parseRegularMessage Msg
--        '4' ->  parseRegularMessage MsgJSON
--        '5' ->  MsgEvent    <$> parseID 
--                            <*> parseEndpoint 
--                            <*> parseEvent
--        '6' ->  try (do 
--                string ":::"
--                n' <- read <$> number
--                char '+'
--                d <- fromString <$> text
--                return $ MsgACK (ID n') (Data d)
--            ) <|> (do
--                string ":::"
--                n' <- read <$> number
--                return $ MsgACK (ID n') NoData
--            )
--        '7' -> colon >> MsgError <$> parseEndpoint <*> parseData
--        '8' ->  return $ MsgNoop
--        _   ->  return $ MsgNoop
--    where   parseRegularMessage ctr = ctr <$> parseID 
--                                          <*> parseEndpoint 
--                                          <*> parseData

----------------------------------------------------------------------------------
--endpoint :: Parser String
--endpoint = many1 $ satisfy (/= ':')

----------------------------------------------------------------------------------
--number :: Parser String
--number = many1 digit

----------------------------------------------------------------------------------
--colon :: Parser Char
--colon = char ':'

----------------------------------------------------------------------------------
--parseID :: Parser ID
--parseID  =  try (colon >> number >>= plus >>= return . IDPlus . read)
--        <|> try (colon >> number          >>= return . ID . read)
--        <|>     (colon >>                     return   NoID)
--        where   plus n = char '+' >> return n 

----------------------------------------------------------------------------------
--parseEndpoint :: Parser Endpoint
--parseEndpoint    =  try (colon >> fromString <$> endpoint >>= return . Endpoint)
--                <|>     (colon >>                             return   NoEndpoint)

----------------------------------------------------------------------------------
--parseData :: Parser Data
--parseData    =  try (colon >> text >>= return . Data . fromString)
--            <|>     (colon >>      return   NoData)

----------------------------------------------------------------------------------
--parseEvent :: Parser Event
--parseEvent = try (do
--                colon
--                t <- text
--                case decode (fromString t) of
--                    Just e -> return e
--                    Nothing -> return NoEvent
--            )
--            <|>     (colon >>          return   NoEvent)


----------------------------------------------------------------------------------
---- | Slashes as delimiters
--textWithoutSlash :: Parser String
--textWithoutSlash = many1 $ satisfy (/= '/')

--slash :: Parser Char
--slash = char '/'

----------------------------------------------------------------------------------
---- | Non-empty text
--text :: Parser String
--text = many1 anyChar

a :: ByteString
a = ":{\"name\":\"hey\",\"args\":[\"haha\"]}"
eventParser :: Parser Event
eventParser = do
    string ":"
    t <- takeTill (== 32)
    case decode (serialize t) of
        Just e  -> return e
        Nothing -> return NoEvent


test p = parseOnly p

------------------------------------------------------------------------------               
-- | Parse given HTTP request
parsePath :: ByteString -> Path
parsePath p = case parseOnly pathParser p of
    Left _  -> WithoutSession "" ""
    Right x -> x 

------------------------------------------------------------------------------
-- | "/:namespace/:protocol/[:transport/:sessionID]" -> Path
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
        sessionID <- takeTillEnd
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