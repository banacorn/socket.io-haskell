{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Event where

import Web.SocketIO.Types

import Control.Monad.Reader

--------------------------------------------------------------------------------
-- | messages carried with the event
--
-- @
-- `on` \"echo\" $ do
--     messages <- reply
--     liftIO $ print messages
-- @
reply :: CallbackM [Text]
reply = ask
