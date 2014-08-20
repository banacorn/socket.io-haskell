--------------------------------------------------------------------------------
-- | Polling (XHR, JSONP) transport
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Transport.Polling where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Connection

--------------------------------------------------------------------------------
import qualified    Network.Wai                     as Wai
import              Control.Monad.Trans             (liftIO)

--------------------------------------------------------------------------------
-- | Wrapped as a HTTP app
httpApp :: Env -> Wai.Application
httpApp env httpHequest continuation = do

    liftIO $ print "fuck"

    return undefined
