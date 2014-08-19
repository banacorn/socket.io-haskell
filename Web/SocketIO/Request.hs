--------------------------------------------------------------------------------
-- | Converts HTTP requests to Socket.IO requests and run them
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Request (extractHTTPRequest, runRequest) where
import              Control.Monad.Trans             (liftIO)

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Protocol

--------------------------------------------------------------------------------
--import              Blaze.ByteString.Builder        (Builder)
--import qualified    Blaze.ByteString.Builder        as Builder
--import qualified    Data.ByteString                 as B
--import              Data.Conduit
--import qualified    Data.Conduit.List               as CL
import qualified    Network.Wai                     as Wai

--------------------------------------------------------------------------------
-- | Run!
runRequest :: (Request -> IO Message) -> Request -> IO ByteString
runRequest runner request = runner request >>= return . serialize

--------------------------------------------------------------------------------
-- | Extracts HTTP requests
extractHTTPRequest :: Wai.Request -> IO Request
extractHTTPRequest request = do


    liftIO $ print (Wai.rawQueryString request)
    let path = parsePath (Wai.rawQueryString request)
    let method = Wai.requestMethod request

    case (method, path) of
        ("GET", (Path _ Nothing)) -> return Handshake
        ("GET", (Path _ (Just sessionID))) -> return (Connect sessionID)
        ("POST", (Path _ (Just sessionID))) -> Wai.requestBody request >>= return . Request sessionID . parseMessage
        (_, (Path _ (Just sessionID))) -> return (Disconnect sessionID)
        _ -> error "error handling http request"

----------------------------------------------------------------------------------
---- | Serialize Messages, frame when necessary.
--serializeMessage :: Conduit Message IO ByteString
--serializeMessage = toByteString 0
--    where   toByteString :: Int -> Conduit Message IO ByteString
--            toByteString i = do
--                m <- await
--                n <- await
--                case (m, n) of
--                    -- []
--                    (Nothing, Nothing) -> yield (serialize (Framed [] :: Framed Message))
--                    -- [m'], singleton
--                    (Just m', Nothing) -> if i == 0
--                        then yield (serialize m') -- true singleton
--                        else yield (frame m') -- just a recursion base case 
--                    -- WTF
--                    (Nothing, Just _ ) -> return ()
--                    -- [m', n'], frame m', leftover n'
--                    (Just m', Just n') -> do
--                        yield (frame m')
--                        leftover n'
--                        toByteString (i + 1)
                        
--            frame b = "ï¿½" <> serialize size <> "ï¿½" <> b'
--                where   b' = serialize b
--                        size = B.length b'

--------------------------------------------------------------------------------
-- | Convert Framed Message to Flush Builder so that `Wai.responseSource` can consume it
--toFlushBuilder :: ByteString -> IO (Flush Builder)
--toFlushBuilder = b do 
--    b <- await
--    case b of
--        Just b' -> do
--            yield $ Chunk (Builder.fromByteString b')
--            toFlushBuilder
--        Nothing -> yield $ Flush