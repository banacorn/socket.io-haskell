{-# LANGUAGE OverloadedStrings #-}
module SocketIO.Request (processRequest) where

import SocketIO.Type
import SocketIO.Util
import SocketIO.Parser

import Control.Applicative ((<$>), (<*>))            
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import qualified Network.Wai as Wai
import Network.HTTP.Types (Method)

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit.List (consume)
import Data.Conduit (($$))
import Data.Monoid (mconcat)

type ProcessedRequest = (Method, Body, Namespace, Protocol, Transport, SessionID)

processHTTPRequest :: Wai.Request -> IO ProcessedRequest
processHTTPRequest request = do
    b <- parseBody request
    return $ case path of
        [n, p]          -> (method, b, n,  p,  "", "")
        [n, p, t]       -> (method, b, n,  p,  t,  "")
        [n, p, t, s]    -> (method, b, n,  p,  t,  s)
        _               -> (method, b, "", "", "", "")
    where   method  = Wai.requestMethod request
            path    = map TL.fromStrict . cleanup . Wai.pathInfo $ request
            cleanup = filter (/= "")

identifyRequest  :: ProcessedRequest -> IO Request

identifyRequest ("GET", _, n, p, "", "") = return Handshake
identifyRequest ("GET", _, n, p, t, s) = return $ Connect s

identifyRequest ("POST", b, n, p, t, s) = do
    print message
    return $ Disconnect s
    where   message = parseMessage b


processRequest r = processHTTPRequest r >>= identifyRequest 


parseBody :: Wai.Request -> IO BL.ByteString
parseBody req = fromByteString . mconcat <$> runResourceT (Wai.requestBody req $$ consume)

--createSession :: SessionM SessionID
--createSession = readTable $ \table -> do
--    sessionID <- genSessionID
--    H.insert table sessionID Connecting
--    return sessionID
--    where   genSessionID = fmap (fromString . show) (randomRIO (0, 99999999999999999999 :: Int)) :: IO Text

--updateSession :: SessionID -> Status -> SessionM ()
--updateSession sessionID status = readTable $ \table -> do
--    H.delete table sessionID
--    H.insert table sessionID status

--deleteSession :: SessionID -> SessionM ()
--deleteSession sessionID = readTable $ \table -> 
--    H.delete table sessionID

--lookupSession :: SessionID -> SessionM Status
--lookupSession sessionID = readTable $ \table -> do
--    result <- H.lookup table sessionID
--    case result of
--        Just status -> return status
--        Nothing     -> return Disconnected
