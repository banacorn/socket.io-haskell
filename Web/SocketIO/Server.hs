--------------------------------------------------------------------------------
-- | Servers, standalone or adapted
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Server
    (   server
    ,   serverConfig
    ,   defaultConfig
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Channel
import              Web.SocketIO.Connection
import              Web.SocketIO.Request
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Monad.Trans             (liftIO)
import qualified    Data.ByteString.Lazy            as BL
import              Network.HTTP.Types              (status200)
import              Network.HTTP.Types.Header       (ResponseHeaders)
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

    -- session table
    tableRef <- newSessionTableRef

    -- output channels
    logChannel      <- newLogChannel
    globalChannel   <- newGlobalChannel
    streamToHandle (logTo config) logChannel

    let vorspann = header config
    let env = Env tableRef handler config logChannel globalChannel

    -- run it with Warp
    Warp.run port (httpApp vorspann (runConnection env))

--------------------------------------------------------------------------------
-- | Wrapped as a HTTP app
httpApp :: ResponseHeaders -> (Request -> IO Message) -> Wai.Application
httpApp vorspann runConnection' httpRequest = liftIO $ do
    req <- processHTTPRequest httpRequest
    response <- runConnection' req
    waiResponse vorspann (serialize response)

--------------------------------------------------------------------------------
-- | Default configurations to be overridden.
        --
        -- > defaultConfig :: Configuration
        -- > defaultConfig = Configuration
        -- >    {   transports = [XHRPolling]
        -- >    ,   logLevel = 2               
        -- >    ,   logTo = stderr        
        -- >    ,   header = 
        -- >            [   ("Content-Type", "text/plain")
        -- >            ,   ("Connection", "keep-alive")
        -- >            ,   ("Access-Control-Allow-Origin", "*") 
        -- >            ]      
        -- >    ,   heartbeats = True
        -- >    ,   closeTimeout = 60
        -- >    ,   heartbeatTimeout = 60
        -- >    ,   heartbeatInterval = 25
        -- >    ,   pollingDuration = 20
        -- >    }
        --
defaultConfig :: Configuration
defaultConfig = Configuration
    {   transports = [XHRPolling]
    ,   logLevel = 2
    ,   logTo = stderr
    ,   header = 
            [   ("Content-Type", "text/plain")
            ,   ("Connection", "keep-alive")
            ,   ("Access-Control-Allow-Origin", "*") 
            ]
    ,   heartbeats = True
    ,   closeTimeout = 60
    ,   heartbeatTimeout = 60
    ,   heartbeatInterval = 25
    ,   pollingDuration = 20
}

--------------------------------------------------------------------------------
-- | Make Wai response
waiResponse :: Monad m => ResponseHeaders -> BL.ByteString -> m Wai.Response
waiResponse vorspann = return . Wai.responseLBS status200 vorspann