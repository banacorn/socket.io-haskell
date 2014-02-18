{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Web.SocketIO.Types.Base
    (   module Web.SocketIO.Types.Log
    ,   module Web.SocketIO.Types.Request
    ,   module Web.SocketIO.Types.SocketIO
    ,   module Web.SocketIO.Types.String
    --,   ConnectionM(..)
    --,   SessionM(..)
    --,   ConnectionLayer(..)
    --,   SessionLayer(..)
    ,   Env(..)
    ,   Session(..)
    ,   SessionState(..)
    ,   SessionAction(..)
    ,   Table
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.Request
import              Web.SocketIO.Types.Log
import              Web.SocketIO.Types.String
import              Web.SocketIO.Types.SocketIO

--------------------------------------------------------------------------------
import              Control.Applicative
import              Control.Concurrent.MVar.Lifted
import              Control.Concurrent.Chan.Lifted
import              Control.Monad.Reader       
import              Control.Monad.Trans.Control
import              Control.Monad.Base

import qualified    Data.HashMap.Strict                     as H
import              Data.IORef.Lifted

--------------------------------------------------------------------------------
type Table = H.HashMap SessionID Session 
data SessionState = Connecting | Connected deriving (Show, Eq)
data SessionAction   = SessionHandshake
                     | SessionConnect
                     | SessionPolling
                     | SessionEmit Event
                     | SessionDisconnect

--------------------------------------------------------------------------------
data Env = Env { 
    envSessionTableRef :: IORef Table, 
    envHandler :: HandlerM (), 
    envConfiguration :: Configuration,
    envLogChannel :: Chan ByteString,
    envGlobalChannel :: Chan Package
}

--------------------------------------------------------------------------------
data Session = Session { 
    sessionSessionID :: SessionID, 
    sessionState :: SessionState, 
    sessionChannelHub :: ChannelHub, 
    sessionListener :: [Listener],
    sessionTimeoutVar :: MVar Bool
}

instance Show Session where
    show (Session i s _ _ _) = "Session " 
                            ++ fromByteString i 
                            ++ " [" ++ show s ++ "]"
