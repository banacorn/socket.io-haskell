{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server (server, serverConfig, defaultConfig) where

import              Web.SocketIO.Util
import              Web.SocketIO.Type
import              Web.SocketIO.Type.Message
import              Web.SocketIO.Type.String
import              Web.SocketIO.Type.Event
import              Web.SocketIO.Type.SocketIO
import              Web.SocketIO.Session
import              Web.SocketIO.Connection
import              Web.SocketIO.Request
import              Web.SocketIO.Event
import              Web.SocketIO.Parser             

import              Control.Concurrent.Chan
import              Control.Applicative             ((<$>))
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

    Warp.runSettings settings (httpApp (runConnection env))

httpApp :: (Request -> IO Text) -> Wai.Application
httpApp runConnection' httpRequest = liftIO $ do
    req <- processHTTPRequest httpRequest
    response <- runConnection' req
    text response

-- dummy test ws app
wsApp :: (Request -> IO Text) -> WS.ServerApp
wsApp runConnection' pending = do

    -- accecpt connection anyway
    conn <- WS.acceptRequest pending

    -- parse path
    let WithSession _ _ _ sessionID = parsePath (WS.requestPath (WS.pendingRequest pending))

    -- fire off the first ping
    runConnection' (Connect True sessionID) >>= WS.sendTextData conn
    --WS.sendTextData conn ("1::" :: Text)
    forever $ do
        message <- parseMessage <$> WS.receiveData conn 
        case message of
            MsgEvent _ _ emitter -> do
                runConnection' (Emit sessionID emitter) >>= WS.sendTextData conn
            _                    -> return ()

        print message
        return ()

defaultConfig :: Configuration
defaultConfig = Configuration
    {   transports = [WebSocket, XHRPolling]
    ,   logLevel = 3
    ,   heartbeats = True
    ,   closeTimeout = 60
    ,   heartbeatTimeout = 60
    ,   heartbeatInterval = 25
    ,   pollingDuration = 20
}

text :: Monad m => Text -> m Wai.Response
text = return . Wai.responseLBS status200 header . fromText

header = [
    ("Content-Type", "text/plain"),
    ("Connection", "keep-alive"),
    ("Access-Control-Allow-Credentials", "true"),
    ("Access-Control-Allow-Origin", "http://localhost:3000") 
    ]