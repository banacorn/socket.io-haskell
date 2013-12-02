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


type RequestInfo = (Method, Path, Message)

retrieveRequestInfo :: Wai.Request -> IO RequestInfo
retrieveRequestInfo request = do

    body <- parseHTTPBody request

    let path = parsePath (Wai.rawPathInfo request)


    return 
        (   Wai.requestMethod request
        ,   path 
        ,   body
        )


processRequestInfo :: RequestInfo -> Request
processRequestInfo ("GET" , (WithoutSession _ _)         , _                   )    = Handshake 
processRequestInfo ("GET" , (WithSession _ _ _ sessionID), _                   )    = Connect sessionID
processRequestInfo ("POST", (WithSession _ _ _ sessionID), MsgEvent _ _ emitter)    = Emit sessionID emitter
processRequestInfo (_     , (WithSession _ _ _ sessionID), _                   )    = Disconnect sessionID
 
processHTTPRequest :: Wai.Request -> IO Request
processHTTPRequest request = fmap processRequestInfo (retrieveRequestInfo request)

parseHTTPBody :: Wai.Request -> IO Message
parseHTTPBody req = parseMessage . fromByteString . mconcat <$> runResourceT (Wai.requestBody req $$ consume)
