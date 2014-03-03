--------------------------------------------------------------------------------
-- | Functions exposed to end users in CallbackM
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Event where

--------------------------------------------------------------------------------
import Web.SocketIO.Types

--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Monad.Reader

--------------------------------------------------------------------------------
-- | Extracts payload carried by the event
--
-- @
-- `on` \"echo\" $ do
--     payload <- reply
--     liftIO $ print payload
--     emit "echo" payload 
-- @
reply :: CallbackM [Text]
reply = do
    Payload p <- callbackEnvPayload <$> ask
    return p

--------------------------------------------------------------------------------
-- | Name of the event
getEventName :: CallbackM EventName
getEventName = callbackEnvEventName <$> ask
