{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server (server, serverConfig, defaultConfig) where

import              Web.SocketIO.Util
import              Web.SocketIO.Type
import              Web.SocketIO.Type.String
import              Web.SocketIO.Type.Event
import              Web.SocketIO.Type.SocketIO
import              Web.SocketIO.Session
import              Web.SocketIO.Connection
import              Web.SocketIO.Request
import              Web.SocketIO.Event
import              Web.SocketIO.Parser             (parseMessage)

import              Control.Concurrent.Chan
import              Control.Concurrent              (forkIO)
import              Control.Monad                   (forever)
import              Control.Monad.Trans             (liftIO)
import              Network.HTTP.Types              (status200)
import qualified    Network.Wai                     as Wai
import qualified    Network.Wai.Handler.Warp        as Warp
import              Network.Wai.Handler.WebSockets  (intercept)
import qualified    Network.WebSockets              as WS

import              Data.ByteString.Lazy            (ByteString)
import qualified    Data.ByteString.Lazy            as BL


server :: Port -> SocketM () -> IO ()
server p h = serverConfig p defaultConfig h

serverConfig :: Port -> Configuration -> SocketM () -> IO ()
serverConfig port config handler = do

    tableRef <- newSessionTable

    stdout <- newChan :: IO (Chan String)

    forkIO . forever $ do
        readChan stdout >>= putStrLn 

    let env = Env tableRef handler config stdout

    -- intercept websocket stuffs
    let settings = Warp.defaultSettings
                        { Warp.settingsPort = port
                        , Warp.settingsIntercept = intercept (wsApp (runConnection env))
                        }

    Warp.runSettings settings $ \httpRequest -> liftIO $ do
        req <- processHTTPRequest httpRequest
        response <- runConnection env req
        text response

-- dummy test ws app
wsApp :: (Request -> IO Text) -> WS.ServerApp
wsApp runConnection' pending = do
    let path = WS.requestPath $ WS.pendingRequest pending 
    conn <- WS.acceptRequest pending

    --reply <- runConnection' RConnect
    print path


    WS.sendTextData conn ("1::" :: Text)
    forever $ do
        raw <- WS.receiveData conn :: IO ByteString
        let msg = parseMessage raw 
        --print msg
        return ()

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