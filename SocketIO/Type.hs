{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SocketIO.Type where

import SocketIO.Util

import qualified Network.Wai as Wai

import Control.Monad.Reader       
import Control.Monad.Writer       
import Control.Concurrent.Chan.Lifted
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Applicative

import qualified Data.HashTable.IO as H
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as Aeson
import Data.IORef.Lifted
import Data.Monoid ((<>))

type Text = TL.Text
type Event = Text
type Reply = [Text]
type SessionID = Text 

type Listener = (Event, CallbackM ())
data Emitter  = Emitter Event Reply | NoEmitter deriving (Show, Eq)

instance Aeson.ToJSON Emitter where
   toJSON (Emitter name args) = Aeson.object ["name" Aeson..= name, "args" Aeson..= args]



type HashTable k v = H.LinearHashTable k v
type Table = HashTable SessionID Session 
data Status = Connecting | Connected | Disconnecting deriving Show
type Buffer = Chan Emitter

data Request    = RHandshake
                | RDisconnect SessionID
                | RConnect SessionID 
                | REmit SessionID Emitter
                deriving (Show)

data SessionState   = Syn
                    | Ack
                    | Polling
                    | Emit Emitter
                    | Disconnect
                    | Error

data Env = Env { getSessionTable :: IORef Table, getHandler :: SocketM () }
newtype ConnectionM a = ConnectionM { runConnectionM :: ReaderT Env IO a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Env, MonadBase IO)


data Session = Session { 
    getSessionID :: SessionID, 
    getStatus :: Status, 
    getBuffer :: Buffer, 
    getListener :: [Listener]
} | NoSession


newtype SessionM a = SessionM { runSessionM :: (ReaderT Session IO) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader Session, MonadBase IO)

instance (MonadBaseControl IO) SessionM where
    newtype StM SessionM a = StMSession { unStMSession :: StM (ReaderT Session IO) a }
    liftBaseWith f = SessionM (liftBaseWith (\run -> f (liftM StMSession . run . runSessionM)))
    restoreM = SessionM . restoreM . unStMSession

newtype SocketM a = SocketM { runSocketM :: (ReaderT Buffer (WriterT [Listener] IO)) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Listener], MonadReader Buffer, MonadBase IO)

newtype CallbackM a = CallbackM { runCallbackM :: (WriterT [Emitter] (ReaderT Reply (ReaderT Buffer IO))) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadWriter [Emitter], MonadReader Reply, MonadBase IO)


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
    toMessage = fromLazyByteString . Aeson.encode 

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