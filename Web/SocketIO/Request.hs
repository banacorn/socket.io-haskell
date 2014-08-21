--------------------------------------------------------------------------------
-- | Converts HTTP requests to Socket.IO requests
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Request (extractHTTPRequest) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Protocol

--------------------------------------------------------------------------------
import qualified    Network.Wai                     as Wai

--------------------------------------------------------------------------------
-- | Extracts HTTP requests
extractHTTPRequest :: Wai.Request -> IO Request_
extractHTTPRequest request = do

    let path = parsePath (Wai.rawQueryString request)
    let method = Wai.requestMethod request

    case (method, path) of
        ("GET", (Path _ Nothing)) -> return Handshake
        ("GET", (Path _ (Just sessionID))) -> return (Connect_ sessionID)
        ("POST", (Path _ (Just sessionID))) -> Wai.requestBody request >>= return . Request_ sessionID . parseMessage
        (_, (Path _ (Just sessionID))) -> return (Disconnect sessionID)
        _ -> error "error handling http request"
