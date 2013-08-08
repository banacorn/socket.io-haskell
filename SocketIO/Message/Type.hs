{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Message.Type (
    State(..), Message(..), Endpoint(..), ID(..), Data(..),
    Msg(..)
) where

--type Text = TL.Text
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text, pack)
import Data.Monoid (mconcat)

(+++) = TL.append

data State = Connecting | Connected | Disconnecting | Disconnected deriving Show

data Message    = Disconnect Endpoint
                | Connect Endpoint
                | Heartbeat
                | Message ID Endpoint Data
                | JSONMessage ID Endpoint Data
                | Event ID Endpoint Data
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
data Data       = Data String
                | NoData
                deriving (Show, Eq)

class Msg m where
    encode :: m -> Text

instance Msg Endpoint where
    encode (Endpoint s) = pack s
    encode NoEndpoint = ""

instance Msg ID where
    encode (ID i) = pack $ show i
    encode (IDPlus i) = pack $ show i ++ "+"
    encode NoID = ""

instance Msg Data where
    encode (Data s) = pack s
    encode NoData = ""

instance Msg Message where
    encode (Disconnect NoEndpoint)  = "0"
    encode (Disconnect e)           = "0::" +++ encode e
    encode (Connect e)              = "1::" +++ encode e
    encode Heartbeat                = undefined
    encode (Message i e d)          = "3:" +++ encode i +++
                                      ":" +++ encode e +++
                                      ":" +++ encode d
    encode (JSONMessage i e d)      = "4:" +++ encode i +++
                                      ":" +++ encode e +++
                                      ":" +++ encode d
    encode (Event i e d)            = "5:" +++ encode i +++
                                      ":" +++ encode e +++
                                      ":" +++ encode d
    encode (ACK i d)                = "6:::" +++ encode i +++ 
                                      "+" +++ encode d
    encode (Error e d)              = "7::" +++ encode e +++ 
                                      ":" +++ encode d
    encode Noop                     = "8:::"