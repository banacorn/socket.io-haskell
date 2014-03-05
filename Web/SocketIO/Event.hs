--------------------------------------------------------------------------------
-- | Functions exposed to end users in CallbackM
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Event where

--------------------------------------------------------------------------------
import Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative ((<$>))
import              Control.Monad.Reader
import qualified    Data.ByteString.Lazy                as BL

{-# DEPRECATED reply "use msg instead" #-}
--------------------------------------------------------------------------------
-- | This function is deprecated; use 'msg' instead
reply :: CallbackM [Text]
reply = do
    Payload p <- callbackEnvPayload <$> ask
    return p

--------------------------------------------------------------------------------
-- | Extracts payload carried by the event
--
-- @
-- `on` \"echo\" $ do
--     payload <- msg
--     liftIO $ print payload
--     emit "echo" payload 
-- @
msg :: CallbackM [Text]
msg = do
    Payload p <- callbackEnvPayload <$> ask
    return p

--------------------------------------------------------------------------------
-- | Lazy ByteString version of `msg`, convenient for Aeson decoding.
msg' :: CallbackM [BL.ByteString]
msg' = do
    Payload p <- callbackEnvPayload <$> ask
    return (map serialize p)

--------------------------------------------------------------------------------
-- | Name of the event
getEventName :: CallbackM EventName
getEventName = callbackEnvEventName <$> ask
