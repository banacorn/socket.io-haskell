# [Socket.IO](http://socket.io) server in Haskell.

## Install

    cabal install socketio

## Usage

Now only stand-alone version is supported.  `WAI` and `Snap` adapter will added in the future.

### Stand-alone server

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.SocketIO

main = server 4000 $ do

    on "ping" $ emit "pong" []
    
    -- reply :: CallbackM [Text]
    on "echo" $ reply >>= emit "pong"
    
    -- do some IO
    on "Kim Jong-Un" $ liftIO launchMissile
    
```

## Supported Transports

`websockets` under development.

* `xhr-polling`
