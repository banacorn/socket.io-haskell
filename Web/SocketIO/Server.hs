{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server (server, serverConfig, defaultConfig) where

import              Web.SocketIO.Util
import              Web.SocketIO.Type
import              Web.SocketIO.Session
import              Web.SocketIO.Connection
import              Web.SocketIO.Request
import              Web.SocketIO.Event

import              Control.Monad.Trans             (liftIO)
import              Network.HTTP.Types              (status200)
import qualified    Network.Wai                     as Wai
import qualified    Network.Wai.Handler.Warp        as Warp
import              Network.Wai.Handler.WebSockets  (intercept)
import qualified    Network.WebSockets              as WS

server :: Port -> SocketM () -> IO ()
server p h = serverConfig p defaultConfig h

serverConfig :: Port -> Configuration -> SocketM () -> IO ()
serverConfig port config handler = do

    tableRef <- newSessionTable

    -- intercept websocket stuffs
    let settings = Warp.defaultSettings
                        { Warp.settingsPort = port
                        , Warp.settingsIntercept = intercept wsApp
                        }

    Warp.runSettings settings $ \httpRequest -> liftIO $ do
        req <- processHTTPRequest httpRequest
        response <- runConnection (Env tableRef handler config) req
        text response

-- dummy test ws app
wsApp :: WS.ServerApp
wsApp pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn ("hello" :: Text)

defaultConfig :: Configuration
defaultConfig = Configuration {
    transports = [WebSocket, XHRPolling],
    logLevel = 3
}

text :: Monad m => Text -> m Wai.Response
text = return . Wai.responseLBS status200 header . fromText

pollingDuration = 20

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]