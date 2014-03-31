--------------------------------------------------------------------------------
-- | Converts HTTP requests to Socket.IO requests
{-# LANGUAGE OverloadedStrings #-}
module Web.SocketIO.Request (parseHTTPRequest, parseWSPath, parseWSBody) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Protocol

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>))   
import qualified    Network.Wai                             as Wai
import qualified    Network.WebSockets                      as WS
import              Network.HTTP.Types                      (Method)

--------------------------------------------------------------------------------
-- | Information of a HTTP reqeust we need
type RequestInfo = (Method, Path, Framed Message)

--------------------------------------------------------------------------------
-- | Extracts from HTTP reqeusts
retrieveRequestInfo :: Wai.Request -> IO RequestInfo
retrieveRequestInfo request = do

    framedMessages <- parseHTTPBody request

    let path = parsePath (Wai.rawPathInfo request)

    return 
        (   Wai.requestMethod request
        ,   path 
        ,   framedMessages
        )

--------------------------------------------------------------------------------
-- | Converts to SocketIO request
processRequestInfo :: RequestInfo -> [Request]
processRequestInfo ("GET" , (WithoutSession _ _)         , _              ) = [Handshake]
processRequestInfo ("GET" , (WithSession _ _ _ sessionID), _              ) = [Connect sessionID]
processRequestInfo ("POST", (WithSession _ _ _ sessionID), Framed messages) = requests
    where   requests = foldr extractEvent [] messages
            extractEvent (MsgEvent _ _ event) acc = Emit sessionID event : acc
            extractEvent _                    acc = acc
processRequestInfo (_     , (WithSession _ _ _ sessionID), _              ) = [Disconnect sessionID]
processRequestInfo _    = error "error parsing http request"

--------------------------------------------------------------------------------
-- | The message part
parseHTTPBody :: Wai.Request -> IO (Framed Message)
parseHTTPBody req = parseFramedMessage <$> Wai.lazyRequestBody req
 
--------------------------------------------------------------------------------
-- | The request part
parseHTTPRequest :: Wai.Request -> IO [Request]
parseHTTPRequest request = processRequestInfo <$> retrieveRequestInfo request

--------------------------------------------------------------------------------
-- | peaks only the RequestHead
parseWSPath :: WS.PendingConnection -> IO Request
parseWSPath pendingConn = do
    let path = parsePath (WS.requestPath (WS.pendingRequest pendingConn))
    case path of
        WithSession _ _ _ sessionID -> return (Connect sessionID)
        _ -> error "error parsing ws request"

--------------------------------------------------------------------------------
parseWSBody :: SessionID -> WS.Connection -> IO Request
parseWSBody sessionID conn = do
    msg <- WS.receiveData conn :: IO LazyByteString
    let Framed [MsgEvent _ _ event] = parseFramedMessage msg
    return (Emit sessionID event)
