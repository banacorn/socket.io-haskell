{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Request (processHTTPRequest) where

import Web.SocketIO.Type
import Web.SocketIO.Type.String
import Web.SocketIO.Type.SocketIO
import Web.SocketIO.Type.Message
import Web.SocketIO.Type.Event

import Web.SocketIO.Util
import Web.SocketIO.Parser

import Control.Applicative          ((<$>), (<*>))   
import Control.Monad                ((>=>))         
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import qualified Network.Wai as Wai
import Network.HTTP.Types           (Method)

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit.List            (consume)
import Data.Conduit                 (($$))
import Data.Monoid                  (mconcat)

type Namespace = Text
type Protocol = Text
--type Body = BL.ByteString


type RequestInfo = (Method, Maybe Namespace, Maybe Protocol, Maybe Transport, Maybe SessionID, Message)

retrieveRequestInfo :: Wai.Request -> IO RequestInfo
retrieveRequestInfo request = do

    body <- parseBody request

    let path = map TL.fromStrict (Wai.pathInfo request)
    let transport = case fmap parseTransport (path `elemAt` 2) of
            Nothing          -> Nothing
            Just NoTransport -> Nothing
            Just t           -> Just t

    return 
        (   Wai.requestMethod request
        ,   path `elemAt` 0
        ,   path `elemAt` 1
        ,   transport
        ,   path `elemAt` 3
        ,   parseMessage body
        )

    where   elemAt :: [a] -> Int -> Maybe a
            elemAt [] _ = Nothing
            elemAt (x:xs) 0 = Just x
            elemAt (x:xs) n = elemAt xs (n-1)

            parseTransport "websocket" = WebSocket
            parseTransport "xhr-polling" = XHRPolling
            parseTransport _ = NoTransport

processRequestInfo :: RequestInfo -> Request
processRequestInfo ("GET" , _, _, Nothing, Nothing       , _                   )    = RHandshake 
processRequestInfo ("GET" , _, _, _      , Just sessionID, _                   )    = RConnect sessionID
processRequestInfo ("POST", _, _, _      , Just sessionID, MsgEvent _ _ emitter)    = REmit sessionID emitter
processRequestInfo (_     , _, _, _      , Just sessionID, _                   )    = RDisconnect sessionID
 
processHTTPRequest :: Wai.Request -> IO Request
processHTTPRequest request = fmap processRequestInfo (retrieveRequestInfo request)

parseBody :: Wai.Request -> IO BL.ByteString
parseBody req = fromByteString . mconcat <$> runResourceT (Wai.requestBody req $$ consume)