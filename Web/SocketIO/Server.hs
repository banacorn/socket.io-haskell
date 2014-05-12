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
import              Data.Conduit
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
httpApp headerFields runConnection' httpRequest = liftIO $ do
    
    let origin = lookupOrigin httpRequest
    let headerFields' = insertOrigin headerFields origin

    let sourceBody = sourceHTTPRequest httpRequest $= runRequest runConnection'

    return $ Wai.responseSource status200 headerFields' sourceBody

    where   lookupOrigin req = case lookup "Origin" (Wai.requestHeaders req) of
                Just origin -> origin
                Nothing     -> "*"
            insertOrigin fields origin = case lookup "Access-Control-Allow-Origin" fields of
                Just _  -> fields
                Nothing -> ("Access-Control-Allow-Origin", origin) : fields
--------------------------------------------------------------------------------
-- | Default configuration.
        --
        -- > defaultConfig = Configuration
        -- >    {   transports = [XHRPolling]
        -- >    ,   logLevel = 2               
        -- >    ,   logTo = stderr        
        -- >    ,   header = [("Access-Control-Allow-Credentials", "true")]      
        -- >    ,   heartbeats = True
        -- >    ,   closeTimeout = 60
        -- >    ,   heartbeatTimeout = 60
        -- >    ,   heartbeatInterval = 25
        -- >    ,   pollingDuration = 20
        -- >    }
        --
-- You can override it like so:
        --
        -- > myConfig = defaultConfig { logLevel = 0 }
        --
-- Unless specified, the header will be modified to enable cross-origin resource sharing (CORS) like this.
        --
        -- > header = 
        -- >    [   ("Access-Control-Allow-Origin", <origin-of-the-reqeust>)]      
        -- >    ,   ("Access-Control-Allow-Credentials", "true")
        -- >    ]      
        --
defaultConfig :: Configuration
defaultConfig = Configuration
    {   transports = [XHRPolling]
    ,   logLevel = 2
    ,   logTo = stderr
    ,   header = [("Access-Control-Allow-Credentials", "true")]
    ,   heartbeats = True
    ,   closeTimeout = 60
    ,   heartbeatTimeout = 60
    ,   heartbeatInterval = 25
    ,   pollingDuration = 20
}