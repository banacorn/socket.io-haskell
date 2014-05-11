--------------------------------------------------------------------------------
-- | Converts HTTP requests to Socket.IO requests and run them
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Request
    (   sourceRequest
    ,   runRequest
    ,   serializeMessage
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Protocol

--------------------------------------------------------------------------------
import              Blaze.ByteString.Builder        (Builder)
import qualified    Blaze.ByteString.Builder        as Builder
import qualified    Data.ByteString                 as B
import              Data.Conduit
import qualified    Data.Conduit.List               as CL
import qualified    Network.Wai                     as Wai

--------------------------------------------------------------------------------
-- | Extracts and identifies Requests from Wai.Request
sourceRequest :: Wai.Request -> Source IO Request
sourceRequest request = do
    let path = parsePath (Wai.rawPathInfo request)
    let method = Wai.requestMethod request

    case (method, path) of
        ("GET", (WithoutSession _ _)) -> yield Handshake
        ("GET", (WithSession _ _ _ sessionID)) -> yield (Connect sessionID)
        ("POST", (WithSession _ _ _ sessionID)) -> do
            Wai.requestBody request $= parseMessage =$= filterMsgEvent sessionID
        (_, (WithSession _ _ _ sessionID)) -> yield (Disconnect sessionID)
        _ -> error "error handling http request"
    where   filterMsgEvent sessionID = do
                message <- await
                case message of
                    Just (MsgEvent _ _ event) -> yield (Emit sessionID event)
                    _ -> return ()

--------------------------------------------------------------------------------
-- | Run every Request
runRequest :: (Request -> IO Message) -> Conduit Request IO Message
runRequest runner = CL.mapM runner

--------------------------------------------------------------------------------
-- | Convert Framed Message to Flush Builder so that `Wai.responseSource` can consume it
serializeMessage :: Conduit Message IO (Flush Builder)
serializeMessage = toByteString =$= toFlushBuilder
    where   toByteString = do
                m <- await
                n <- await
                case (m, n) of
                    -- []
                    (Nothing, Nothing) -> yield (serialize (Framed [] :: Framed Message))
                    -- [m'], singleton
                    (Just m', Nothing) -> yield (serialize m')
                    -- WTF
                    (Nothing, Just _ ) -> return ()
                    -- [m', n'], frame m', leftover n'
                    (Just m', Just n') -> do
                        yield ("�" <> serialize size <> "�" <> m'')
                        leftover n'
                        toByteString
                        where   m'' = serialize m'
                                size = B.length m''
            toFlushBuilder = do
                b <- await
                case b of
                    Just b' -> yield $ Chunk (Builder.fromByteString b')
                    Nothing -> yield $ Flush