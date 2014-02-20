{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server
    (   server
    ,   serverConfig
    ,   defaultConfig
    ) where

import              Web.SocketIO.Channel
import              Web.SocketIO.Connection
import              Web.SocketIO.Request
import              Web.SocketIO.Types

import              Control.Monad.Trans             (liftIO)
import              Network.HTTP.Types              (status200)
import qualified    Network.Wai                     as Wai
import qualified    Network.Wai.Handler.Warp        as Warp


--------------------------------------------------------------------------------
-- | Run a socket.io application, build on top of Warp.
server :: Port -> HandlerM () -> IO ()
server p h = serverConfig p defaultConfig h

--------------------------------------------------------------------------------
-- | Run a socket.io application with configurations applied.
serverConfig :: Port -> Configuration -> HandlerM () -> IO ()
serverConfig port config handler = do

    tableRef <- newSessionTableRef

    logChannel      <- newLogChannel
    globalChannel   <- newGlobalChannel

    streamToStderr logChannel

    let env = Env tableRef handler config logChannel globalChannel

    Warp.run port (httpApp (runConnection env))

--------------------------------------------------------------------------------
httpApp :: (Request -> IO Message) -> Wai.Application
httpApp runConnection' httpRequest = liftIO $ do
    req <- processHTTPRequest httpRequest
    response <- runConnection' req
    text (serialize response)

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