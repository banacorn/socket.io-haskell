{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server
    (   server
    ,   serverConfig
    ,   defaultConfig
    ) where

import              Web.SocketIO.Util
import              Web.SocketIO.Types
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

import              Data.ByteString.Lazy            (ByteString)
import qualified    Data.ByteString.Lazy            as BL

--------------------------------------------------------------------------------
-- | Run a socket.io application, build on top of Warp.
server :: Port -> HandlerM () -> IO ()
server p h = serverConfig p defaultConfig h

--------------------------------------------------------------------------------
-- | Run a socket.io application with configurations applied.
serverConfig :: Port -> Configuration -> HandlerM () -> IO ()
serverConfig port config handler = do

    tableRef <- newSessionTable

    stdout <- newChan :: IO (Chan String)

    forkIO . forever $ do
        readChan stdout >>= putStrLn 

    let env = Env tableRef handler config stdout

    Warp.run port (httpApp (runConnection env))

--------------------------------------------------------------------------------
httpApp :: (Request -> IO Text) -> Wai.Application
httpApp runConnection' httpRequest = liftIO $ do
    req <- processHTTPRequest httpRequest
    response <- runConnection' req
    text response

--------------------------------------------------------------------------------
-- | Default configurations to be overridden.
        --
        -- > defaultConfig :: Configuration
        -- > defaultConfig = Configuration
        -- >    {   transports = [XHRPolling]
        -- >    ,   logLevel = 3                
        -- >    ,   closeTimeout = 60
        -- >    ,   pollingDuration = 20
        -- >    ,   heartbeats = True
        -- >    ,   heartbeatTimeout = 60
        -- >    ,   heartbeatInterval = 25
        -- >    }
        --
defaultConfig :: Configuration
defaultConfig = Configuration
    {   transports = [XHRPolling]
    ,   logLevel = 3
    ,   heartbeats = True
    ,   closeTimeout = 60
    ,   heartbeatTimeout = 60
    ,   heartbeatInterval = 25
    ,   pollingDuration = 20
}

--------------------------------------------------------------------------------
text :: Monad m => Text -> m Wai.Response
text = return . Wai.responseLBS status200 header . fromText
    where
            header = [
                ("Content-Type", "text/plain"),
                ("Connection", "keep-alive"),
                ("Access-Control-Allow-Credentials", "true"),
                ("Access-Control-Allow-Origin", "http://localhost:3000") 
                ]