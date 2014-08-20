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

--------------------------------------------------------------------------------
-- | Wrapped as a HTTP app
httpApp :: Env -> Wai.Application
httpApp env httpHequest continuation = do

    body <- Wai.requestBody httpHequest 

    let request = parse requestP (Wai.rawQueryString httpHequest)
    let request' = request { reqBody = body }

    payload <- runConnection env request

    liftIO $ print payload


    return undefined
