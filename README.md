# Socket.io-Haskell

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
