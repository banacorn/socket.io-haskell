{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Type (
    Socket(..),
    Event, Handler, EventMap, Emitter, SocketM(..),
    SocketIOState(..), Message(..), Endpoint(..), ID(..), Data(..),
    Msg(..)
) where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import Data.Monoid (mconcat)
import Control.Monad (mapM_)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

newtype Socket = Socket { socket :: SessionID }

--

type Text = TL.Text
type Event = Text
type SessionID = Text
type Handler = Event -> IO ()
type EventMap = Map.Map Event [Handler]

type Emitter = Event 

newtype SocketM a = SocketM { runSocketM :: WriterT [Emitter] (StateT EventMap Identity) a }
    deriving (Monad, Functor, MonadState EventMap)

--

(+++) = TL.append

data SocketIOState = Connecting | Connected | Disconnecting | Disconnected deriving Show

data Message    = Disconnect Endpoint
                | Connect Endpoint
                | Heartbeat
                | Msg ID Endpoint Data
                | JSONMsg ID Endpoint Data
                | EventMsg ID Endpoint Data
                | ACK ID Data
                | Error Endpoint Data
                | Noop
                deriving (Show, Eq)

data Endpoint   = Endpoint String
                | NoEndpoint
                deriving (Show, Eq)
data ID         = ID Int
                | IDPlus Int
                | NoID
                deriving (Show, Eq)
data Data       = Data Text
                | NoData
                deriving (Show, Eq)

class Msg m where
    encode :: m -> Text

instance Msg Endpoint where
    encode (Endpoint s) = TL.pack s
    encode NoEndpoint = ""

instance Msg ID where
    encode (ID i) = TL.pack $ show i
    encode (IDPlus i) = TL.pack $ show i ++ "+"
    encode NoID = ""

instance Msg Data where
    encode (Data s) = s
    encode NoData = ""

instance Msg Message where
    encode (Disconnect NoEndpoint)  = "0"
    encode (Disconnect e)           = "0::" +++ encode e
    encode (Connect e)              = "1::" +++ encode e
    encode Heartbeat                = undefined
    encode (Msg i e d)              = "3:" +++ encode i +++
                                      ":" +++ encode e +++
                                      ":" +++ encode d
    encode (JSONMsg i e d)          = "4:" +++ encode i +++
                                      ":" +++ encode e +++
                                      ":" +++ encode d
    encode (EventMsg i e d)         = "5:" +++ encode i +++
                                      ":" +++ encode e +++
                                      ":" +++ encode d
    encode (ACK i d)                = "6:::" +++ encode i +++ 
                                      "+" +++ encode d
    encode (Error e d)              = "7::" +++ encode e +++ 
                                      ":" +++ encode d
    encode Noop                     = "8:::"