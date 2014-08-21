--------------------------------------------------------------------------------
-- | Polling (XHR, JSONP) transport
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Transport.Polling where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Parser
import              Web.SocketIO.Connection
--import              Web.SocketIO.Protocol
--import              Web.SocketIO.Protocol

--------------------------------------------------------------------------------
import qualified    Network.Wai                     as Wai
import              Control.Monad.Trans             (liftIO)
import              Network.HTTP.Types              (status200)

import qualified    Data.ByteString.Lazy                 as LB

--------------------------------------------------------------------------------
-- | Wrapped as a HTTP app
httpApp :: Env -> Wai.Application
httpApp env httpRequest continuation = do

    body <- Wai.requestBody httpRequest 

    let request = parse requestP (Wai.rawQueryString httpRequest)
    let request' = request { reqBody = body }

    payload@(Payload sessionID _) <- runConnection env request'

    let serializedPayload = serialize payload
    let contentLength = serialize (fromEnum $ LB.length serializedPayload)

    liftIO $ print request'
    liftIO $ print serializedPayload
    liftIO $ print contentLength

    let origin = lookupOrigin httpRequest
    let responseHeaders = (header $ envConfiguration env)  -==|- ("Access-Control-Allow-Origin", origin) 
                                            -==|- ("Connection", "keep-alive")
                                            -==|- ("Set-Cookie", "io=" <> sessionID)
                                            -==|- ("Content-Length", contentLength)
                                            -==|- ("Content-Type", "application/octet-stream")

    continuation (Wai.responseLBS status200 responseHeaders serializedPayload)

    where   lookupOrigin req = case lookup "Origin" (Wai.requestHeaders req) of
                Just origin -> origin
                Nothing     -> "*"

            -- inject header only when absent
            headers -==|- (name, value) = case lookup name headers of
                Just _ -> headers -- already in headers
                Nothing -> (name, value) : headers