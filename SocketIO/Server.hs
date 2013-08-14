{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Server where

import SocketIO.Util

import Network.Wai
import Network.Wai.Handler.Warp    (run)
import Network.HTTP.Types          (status200, Method)

import Control.Monad.Trans         (liftIO)

import qualified Data.Text                   as T
import Data.Monoid                 (mconcat)
import Data.List                   (intersperse)
--import Data.Monoid (mconcat)

type Text = T.Text
type Namespace = Text
type Protocol = Text
type Transport = Text
type SessionID = Text 
data SocketRequest = SocketRequest Method Namespace Protocol Transport SessionID deriving (Show)

data Connection = Handshake | Connection | Disconnection deriving Show 


processRequest :: Request -> SocketRequest
processRequest request = case path of
    [n, p]          -> SocketRequest method n  p  "" ""
    [n, p, t]       -> SocketRequest method n  p  t  ""
    [n, p, t, s]    -> SocketRequest method n  p  t  s
    _               -> SocketRequest method "" "" "" ""
    where   method  = requestMethod request
            path    = cleanup $ pathInfo request
            cleanup = filter (/= "")

processSocketRequest :: SocketRequest -> Connection
processSocketRequest (SocketRequest _ "" "" "" "") = Disconnection  
processSocketRequest (SocketRequest "GET" n p "" "") = Handshake  
processSocketRequest (SocketRequest "GET" n p t s) = Connection  
processSocketRequest (SocketRequest _ _ _ _ _) = Disconnection  

preprocess = processSocketRequest . processRequest
server Handshake = return $ string "123:60:60:xhr-polling" 
server _ = return $ string "1::" 
string = responseLBS status200 header

    --case referer of
    --    Nothing -> return $ ResponseFile status200 [("Content-Type", "text/html")] "index.html" Nothing
    --    Just _  -> return $ ResponseFile status200 [] path Nothing
    --where   path = tail . tail . init . show . rawPathInfo $ request
    --        referer = lookup "Referer" (requestHeaders request)
 
main = run 4000 $ server . preprocess

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]