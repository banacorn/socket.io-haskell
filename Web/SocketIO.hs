--------------------------------------------------------------------------------
-- | Socket.IO for Haskell folks.

module Web.SocketIO
    (
        -- * How to use this module
        -- |
        --
        -- Note that most of the string literals below are of type Lazy Text.
        --
        -- >{-# LANGUAGE OverloadedStrings #-}
        -- >
        -- >import Web.SocketIO
        -- >
        -- >-- listens to port 4000
        -- >main = server 4000 $ do
        -- >
        -- >    -- send something to the client
        -- >    emit "some event" ["hey"]
        -- >
        -- >    -- ping-pong
        -- >    on "ping" $ do
        -- >        emit "pong" []
        -- >
        -- >    -- do some IO
        -- >    on "Kim Jong-Un" $ liftIO launchMissile
        -- >        
        -- >    -- broadcast
        -- >    broadcast "UN" "North Korea is best Korea"

        -- * Running a standalone server
        server
    ,   serverConfig
    ,   defaultConfig
    ,   Configuration(..)
    ,   Port
    ,   Transport(XHRPolling, WebSocket)
        
        -- * Sending and receiving events
    ,   Subscriber(..)
    ,   Publisher(..)
    ,   reply
    ,   msg
    ,   msg'
    ,   getEventName
    ,   HasSessionID(..)
    ,   EventName
    ,   SessionID

        -- ** Special events
        -- | On disconnection
        -- 
        -- @
        -- 'on' \"disconnect\" $ do
        --     liftIO $ print \"client disconnected\"
        -- @

        -- * Types
    ,   HandlerM
    ,   CallbackM
    
    ) where

import Web.SocketIO.Types
import Web.SocketIO.Server
import Web.SocketIO.Event