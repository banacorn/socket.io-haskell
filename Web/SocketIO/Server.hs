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
import              Blaze.ByteString.Builder        (Builder)
import qualified    Blaze.ByteString.Builder        as Builder
import              Data.Conduit
import qualified    Data.Conduit.List               as CL
import qualified    Data.ByteString               as B
import              Network.HTTP.Types              (Status, status200, status403)
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


banana :: Wai.Request -> (Request -> IO Message) -> Source IO (Flush Builder)
banana request runConnection' = sourceRequest request $= CL.mapM runConnection' =$= serializeMessage =$= toFlushBuilder
    where   serializeMessage = do
                m <- await
                n <- await
                case (m, n) of
                    (Nothing, Nothing) -> yield (serialize (Framed [] :: Framed Message))
                    (Just m', Nothing) -> yield (serialize m')
                    (Just m', Just n') -> do
                        yield ("�" <> serialize size <> "�" <> m'')
                        leftover n'
                        serializeMessage
                        where   m'' = serialize m'
                                size = B.length m''
            toFlushBuilder = do
                b <- await
                case b of
                    Just b' -> yield $ Chunk $ Builder.fromByteString b'
                    Nothing -> yield $ Flush
--------------------------------------------------------------------------------
-- | Wrapped as a HTTP app
httpApp :: ResponseHeaders -> (Request -> IO Message) -> Wai.Application
httpApp headerFields runConnection' httpRequest = liftIO $ do
    
    let origin = lookupOrigin httpRequest
    let headerFields' = insertOrigin headerFields origin

    return $ Wai.responseSource status200 headerFields' (banana httpRequest runConnection')

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

--------------------------------------------------------------------------------
-- | Make Wai response
waiResponse :: Monad m => ResponseHeaders -> [Message] -> m Wai.Response
waiResponse vorspann messages = do
    return . Wai.responseLBS status vorspann . serialize . Framed $ messages
    where   status = if status403 `elem` (map toHTTPStatus messages)
                        then status403
                        else status200

--------------------------------------------------------------------------------
-- | Maps SocketIO respond message to HTTP status code
toHTTPStatus :: Message -> Status
toHTTPStatus (MsgError _ _) = status403
toHTTPStatus _ = status200