{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Server where

import SocketIO.Util
import SocketIO.Type
import SocketIO.Session
import SocketIO.Connection
import SocketIO.Request
import SocketIO.Event

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp     (run)
import Network.HTTP.Types           (status200)
import Control.Monad.Trans          (liftIO)

server :: SocketM () -> IO ()
server handler = do
    run 4000 $ \httpRequest -> liftIO $ do
        req <- processRequest httpRequest
        response <- runConnection handler req
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