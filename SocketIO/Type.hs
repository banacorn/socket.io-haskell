{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Type where

import SocketIO.Util

import qualified Network.Wai as Wai

import Control.Monad.Reader       
import Control.Concurrent.MVar    

import qualified Data.HashTable.IO as H
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import Data.Monoid ((<>))



--

type Text = TL.Text
type Event = Text
type Reply = [Text]
type Namespace = Text
type Protocol = Text
type Transport = Text
type SessionID = Text 
type Body = BL.ByteString

type HashTable k v = H.LinearHashTable k v
data Status = Connecting | Connected | Disconnecting | Disconnected deriving Show
type Session = (SessionID, Status)
type Table = HashTable SessionID Status 
--data SocketRequest = SocketRequest Method Body (Namespace, Protocol, Transport, SessionID) deriving (Show)

data Request = Handshake | Disconnect SessionID | Connect SessionID | Emit SessionID Trigger deriving (Show)


--data Connection = Handshake | Connection SessionID | Packet SessionID Body | Disconnection deriving Show 

data Local = Local { getToilet :: MVar Wai.Response }
data Env = Env { getSessionTable :: IORef Table }

newtype SessionM a = SessionM { runSessionM :: (ReaderT Env (ReaderT Local IO)) a }
    deriving (Monad, Functor, MonadIO, MonadReader Env)

data Message    = MsgDisconnect Endpoint
                | MsgConnect Endpoint
                | MsgHeartbeat
                | Msg ID Endpoint Data
                | MsgJSON ID Endpoint Data
                | MsgEvent ID Endpoint Trigger
                | MsgACK ID Data
                | MsgError Endpoint Data
                | MsgNoop
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

data Trigger    = Trigger { name :: Event, args :: Reply } 
                | NoTrigger
                deriving (Show, Eq)


class Msg m where
    toMessage :: m -> Text

instance Msg Endpoint where
    toMessage (Endpoint s) = TL.pack s
    toMessage NoEndpoint = ""

instance Msg ID where
    toMessage (ID i) = TL.pack $ show i
    toMessage (IDPlus i) = TL.pack $ show i ++ "+"
    toMessage NoID = ""

instance Msg Data where
    toMessage (Data s) = s
    toMessage NoData = ""

instance Msg Trigger where
    toMessage = fromString . show
instance Msg Message where
    toMessage (MsgDisconnect NoEndpoint)    = "0"
    toMessage (MsgDisconnect e)             = "0::" <> toMessage e
    toMessage (MsgConnect e)                = "1::" <> toMessage e
    toMessage MsgHeartbeat                  = undefined
    toMessage (Msg i e d)                   = "3:" <> toMessage i <>
                                              ":" <> toMessage e <>
                                              ":" <> toMessage d
    toMessage (MsgJSON i e d)               = "4:" <> toMessage i <>
                                              ":" <> toMessage e <>
                                              ":" <> toMessage d
    toMessage (MsgEvent i e d)              = "5:" <> toMessage i <>
                                              ":" <> toMessage e <>
                                              ":" <> toMessage d
    toMessage (MsgACK i d)                  = "6:::" <> toMessage i <> 
                                              "+" <> toMessage d
    toMessage (MsgError e d)                = "7::" <> toMessage e <> 
                                              ":" <> toMessage d
    toMessage MsgNoop                       = "8:::"