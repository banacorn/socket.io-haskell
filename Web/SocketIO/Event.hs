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
-- | Messages carried with the event
--
-- @
-- `on` \"echo\" $ do
--     messages <- reply
--     liftIO $ print messages
-- @
reply :: CallbackM [Payload]
reply = callbackEnvPayload <$> ask

--------------------------------------------------------------------------------
-- | Get EventName
getEventName :: CallbackM EventName
getEventName = callbackEnvEventName <$> ask
