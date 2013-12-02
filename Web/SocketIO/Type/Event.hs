{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Type.Event where

import Web.SocketIO.Type.String

import qualified Data.Aeson as Aeson

type Event = Text
type Reply = [Text]

data Emitter  = Emitter Event Reply | NoEmitter deriving (Show, Eq)

instance Aeson.ToJSON Emitter where
   toJSON (Emitter name args) = Aeson.object ["name" Aeson..= name, "args" Aeson..= args]