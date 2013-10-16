{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server where

import Web.SocketIO.Util
import Web.SocketIO.Type
import Web.SocketIO.Session
import Web.SocketIO.Connection
import Web.SocketIO.Request
import Web.SocketIO.Event

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp     (run)
import Network.HTTP.Types           (status200)
import Control.Monad.Trans          (liftIO)

server :: Port -> SocketM () -> IO ()
server port handler = do
    tableRef <- newSessionTable

    run port $ \httpRequest -> liftIO $ do
        req <- processRequest httpRequest
        response <- runConnection (Env tableRef handler) req
        text response

text :: Monad m => Text -> m Wai.Response
text = return . Wai.responseLBS status200 header . fromText



pollingDuration = 20

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]