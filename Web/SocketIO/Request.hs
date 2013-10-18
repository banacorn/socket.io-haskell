{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Request (processRequest) where

import Web.SocketIO.Type
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
type Body = BL.ByteString
type ProcessedRequest = (Method, Body, Namespace, Protocol, Transport, SessionID)

processHTTPRequest :: Wai.Request -> IO ProcessedRequest
processHTTPRequest request = do
    b <- parseBody request
    return $ case path of
        [n, p]          -> (method, b, n,  p,  NoTransport, "")
        [n, p, t]       -> (method, b, n,  p,  parseTransport t,  "")
        [n, p, t, s]    -> (method, b, n,  p,  parseTransport t,  s)
        _               -> (method, b, "", "", NoTransport, "")
    where   method  = Wai.requestMethod request
            path    = map TL.fromStrict . cleanup . Wai.pathInfo $ request
            parseTransport "websocket" = WebSocket
            parseTransport "xhr-polling" = XHRPolling
            parseTransport _ = NoTransport
            cleanup = filter (/= "")

identifyRequest  :: ProcessedRequest -> IO Request

identifyRequest ("GET", _, _, _, NoTransport, "") = return RHandshake
identifyRequest ("GET", _, _, _, _, s) = return $ RConnect s
identifyRequest ("POST", b, n, p, t, s) = do
    return $ case message of
        MsgEvent i e t  -> REmit s t
        MsgDisconnect _ -> RDisconnect s 
        _               -> RDisconnect s
    where   message = parseMessage b

identifyRequest (_, _, _, _, _, s) = return $ RDisconnect s

processRequest :: Wai.Request -> IO Request
processRequest = processHTTPRequest >=> identifyRequest 

parseBody :: Wai.Request -> IO BL.ByteString
parseBody req = fromByteString . mconcat <$> runResourceT (Wai.requestBody req $$ consume)