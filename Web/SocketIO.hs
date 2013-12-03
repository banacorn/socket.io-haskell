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

        -- * Running a standalone server
        server
    ,   serverConfig
    ,   defaultConfig
    ,   Configuration(..)
    ,   Port
    ,   Transport(XHRPolling)
        
        -- * Binding and triggering events
    ,   Subscriber(..)
    ,   Publisher(..)
    ,   reply
    ,   Event

        -- * Types
    ,   HandlerM
    ,   CallbackM
    
    ) where

import Web.SocketIO.Types
import Web.SocketIO.Server
import Web.SocketIO.Event
