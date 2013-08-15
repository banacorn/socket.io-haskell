module SocketIO.Parser (parseMessage) where

import SocketIO.Type
import SocketIO.Util
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

parseJSON :: Message -> Message
parseJSON (MsgEvent i e (Data d)) = case decodeTrigger bytestring of
    Just t  -> MsgEvent i e (EventData t)
    Nothing -> MsgEvent i e NoData
    where   bytestring = fromText d
parseJSON n = n

parseMessage :: LB.ByteString -> Message
parseMessage text = parseJSON $ case parse parseMessage' "" string of
    Left _  -> MsgNoop
    Right x -> x
    where   string = fromLazyByteString text

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
        '5' ->  parseRegularMessage MsgEvent
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

endpoint = many1 $ satisfy (/= ':')
text = many1 anyChar
number = many1 digit
colon = char ':'

parseID :: Parser ID
parseID  =  try (colon >> number >>= plus >>= return . IDPlus . read)
        <|> try (colon >> number          >>= return . ID . read)
        <|>     (colon >>                     return   NoID)
        where   plus n = char '+' >> return n 

parseEndpoint :: Parser Endpoint
parseEndpoint    =  try (colon >> endpoint >>= return . Endpoint)
                <|>     (colon >>              return   NoEndpoint)

parseData :: Parser Data
parseData    =  try (colon >> text >>= return . Data . fromString)
            <|>     (colon >>          return   NoData)

