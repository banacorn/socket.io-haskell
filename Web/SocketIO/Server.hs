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
import              Web.SocketIO.Transport.Polling
import              Web.SocketIO.Request
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Monad.Trans             (liftIO)
import              Network.HTTP.Types              (status200)
import              Network.HTTP.Types.Header       (ResponseHeaders)
import qualified    Network.Wai                     as Wai
import qualified    Network.Wai.Handler.Warp        as Warp

import qualified    Data.ByteString                 as B

--------------------------------------------------------------------------------
-- | Run a socket.io application, build on top of Warp.
server :: Port -> HandlerM () -> IO ()
server = serverConfig defaultConfig

--------------------------------------------------------------------------------
-- | Run a socket.io application with configurations applied.
serverConfig :: Configuration -> Port -> HandlerM () -> IO ()
serverConfig config port handler = do

    -- session table
    tableRef <- newSessionTableRef

    -- output channels
    logChannel      <- newLogChannel
    globalChannel   <- newGlobalChannel
    streamToHandle (logTo config) logChannel

    let env = Env tableRef handler config logChannel globalChannel

    -- run it with Warp
    --Warp.run port (httpApp config (runConnection env))
    Warp.run port (httpApp env)


----------------------------------------------------------------------------------
---- | Wrapped as a HTTP app
--httpApp :: ResponseHeaders -> (Request -> IO Message) -> Wai.Application
--httpApp responseHeaders runConnection' httpRequest respond = do
    
--    -- http response body
--    message <- extractHTTPRequest httpRequest >>= runConnection'
    
--    let body = serialize message
--    let origin = lookupOrigin httpRequest

--    let contentLength = B.length body

--    -- http response headers
--    let responseHeaders' = responseHeaders  -==|- ("Access-Control-Allow-Origin", origin) 
--                                            -==|- ("Connection", "keep-alive")
--                                            -==|- ("Set-Cookie", "io=tDbNkKKtAahP1yFkAAAC")
--                                            -==|- ("Content-Length", serialize contentLength)
--                                            -==|- ("Content-Type", "application/octet-stream")

--    respond (Wai.responseLBS status200 responseHeaders' (serialize body))

--    where   lookupOrigin req = case lookup "Origin" (Wai.requestHeaders req) of
--                Just origin -> origin
--                Nothing     -> "*"

--            -- inject header only when absent
--            headers -==|- (name, value) = case lookup name headers of
--                Just _ -> headers -- already in headers
--                Nothing -> (name, value) : headers
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
    ,   pingTimeout = 60000
    ,   pingInterval = 25000
    ,   maxHttpBufferSize = 10000000
    ,   allowUpgrades = True
    ,   cookie = Just "io"
}