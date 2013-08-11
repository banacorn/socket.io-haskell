module SocketIO.Parser (parseMessage) where

import SocketIO.Type
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BS


parseMessage :: BS.ByteString -> Message
parseMessage text = case parse parseMessage' "" string of
    Left _ -> Noop
    Right x -> x
    where   string = BSC.unpack $ BS.toStrict text

parseMessage' :: Parser Message
parseMessage' = do
    n <- digit
    case n of
        '0' ->  (parseEndpoint >>= return . Disconnect)
            <|> (                  return $ Disconnect NoEndpoint)
        '1' ->  (parseEndpoint >>= return . Connect)
            <|> (                  return $ Connect NoEndpoint)
        '2' ->  (return Heartbeat)
        '3' ->  parseRegularMessage Msg
        '4' ->  parseRegularMessage JSONMsg
        '5' ->  parseRegularMessage EventMsg
        '6' ->  try (do 
                string ":::"
                n <- read <$> number
                char '+'
                d <- TL.pack <$> text
                return $ ACK (ID n) (Data d)
            ) <|> (do
                string ":::"
                n <- read <$> number
                return $ ACK (ID n) NoData
            )
        '7' -> colon >> Error <$> parseEndpoint <*> parseData
        '8' ->  return $ Noop
        _   ->  return $ Noop
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
parseData    =  try (colon >> text >>= return . Data . TL.pack)
            <|>     (colon >>          return   NoData)
