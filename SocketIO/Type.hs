{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SocketIO.Type where

import SocketIO.Util

import qualified Network.Wai as Wai

import Control.Monad.Reader       
import Control.Monad.Writer       
import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Applicative

import qualified Data.HashTable.IO as H
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef.Lifted
import Data.Monoid ((<>))

type Text = TL.Text
type Event = Text
type Reply = [Text]
type SessionID = Text 

type Listener = (Event, CallbackM ())
data Emitter  = Emitter Event Reply | NoEmitter deriving (Show, Eq)


type HashTable k v = H.LinearHashTable k v
type Table = HashTable SessionID Session 
data Status = Connecting | Connected | Disconnecting | Disconnected deriving Show
type Buffer = MVar Text
data Session = Session { status :: Status, buffer :: Buffer } | NoSession


data Request = Handshake | Disconnect SessionID | Connect SessionID | Emit SessionID Emitter deriving (Show)

--data Local = Local { getToilet :: MVar Wai.Response }
data Env = Env { getSessionTable :: IORef Table }

newtype SessionM a = SessionM { runSessionM :: (ReaderT Env IO) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Env, MonadBase IO)

instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMEnv { unStMEnv :: StM (ReaderT Env IO) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMEnv . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMEnv

newtype SocketM a = SocketM { runSocketM :: (WriterT [Emitter] (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, MonadIO, MonadWriter [Emitter])

newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT Reply IO)) a }
    deriving (Monad, Functor, MonadIO, MonadWriter [Emitter], MonadReader Reply)



data Message    = MsgDisconnect Endpoint
                | MsgConnect Endpoint
                | MsgHeartbeat
                | Msg ID Endpoint Data
                | MsgJSON ID Endpoint Data
                | MsgEvent ID Endpoint Emitter
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

instance Msg Emitter where
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