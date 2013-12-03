module Web.SocketIO
    (
        -- * How to use this module
        -- |
        --
        -- Note that most of the string literals below are of type Text.
        --
        -- >{-\# LANGUAGE OverloadedStrings \#-}
        -- >
        -- >import Web.SocketIO
        -- >
        -- >main = server 4000 $ do
        -- >
        -- >    emit "some event" ["hey"]
        -- >
        -- >    on "ping" $ do
        -- >        emit "pong" []

        -- * Running a standalone server
        server
        -- | Run a socket.io application, build on top of Warp.
    ,   serverConfig
        -- | Run a socket.io application with configurations applied.
    ,   defaultConfig
        -- | Default configurations to be overridden.
        --
        -- > defaultConfig :: Configuration
        -- > defaultConfig = Configuration
        -- >    {   transports = [XHRPolling]
        -- >    ,   logLevel = 3
        -- >    ,   closeTimeout = 60
        -- >    ,   pollingDuration = 20
        -- >    ,   heartbeats = True
        -- >    ,   heartbeatTimeout = 60
        -- >    ,   heartbeatInterval = 25
        -- >    }
        --
    ,   Configuration(..)
    ,   Port
        -- * Binding and triggering events
    ,   on
    ,   emit
    ,   reply
    ,   Event
    ,   Publisher(..)
    ,   Sbuscriber(..)
    ,   Transport(..)
        -- * Types
    ,   SocketM
    ,   CallbackM
    ) where

import Web.SocketIO.Types
import Web.SocketIO.Server
import Web.SocketIO.Event
