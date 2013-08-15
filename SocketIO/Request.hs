{-# LANGUAGE OverloadedStrings #-}
module SocketIO.Request (processRequest) where

import SocketIO.Type
import SocketIO.Util
import SocketIO.Parser

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
type Transport = Text
type Body = BL.ByteString
type ProcessedRequest = (Method, Body, Namespace, Protocol, Transport, SessionID)

processHTTPRequest :: Wai.Request -> IO ProcessedRequest
processHTTPRequest request = do
    b <- parseBody request
    return $ case path of
        [n, p]          -> (method, b, n,  p,  "", "")
        [n, p, t]       -> (method, b, n,  p,  t,  "")
        [n, p, t, s]    -> (method, b, n,  p,  t,  s)
        _               -> (method, b, "", "", "", "")
    where   method  = Wai.requestMethod request
            path    = map TL.fromStrict . cleanup . Wai.pathInfo $ request
            cleanup = filter (/= "")

identifyRequest  :: ProcessedRequest -> IO Request

identifyRequest ("GET", _, n, p, "", "") = return Handshake
identifyRequest ("GET", _, n, p, t, s) = return $ Connect s

identifyRequest ("POST", b, n, p, t, s) = do
    return $ case message of
        MsgEvent i e t  -> Emit s t
        MsgDisconnect _ -> Disconnect s 
        _ -> Disconnect s
    where   message = parseMessage b

identifyRequest (_, _, _, _, _, s) = return $ Disconnect s

processRequest :: Wai.Request -> IO Request
processRequest = processHTTPRequest >=> identifyRequest 

parseBody :: Wai.Request -> IO BL.ByteString
parseBody req = fromByteString . mconcat <$> runResourceT (Wai.requestBody req $$ consume)