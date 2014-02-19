{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Event
    (   Event(..)
    ,   EventName
    ,   EventType(..)
    ,   Payload
    ,   Package
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String

----------------------------------------------------------------------------------
import              Control.Applicative
import              Data.Aeson                              

--------------------------------------------------------------------------------
-- | Event
type SessionID = ByteString
type EventName = Text
type Payload = Text
data Event = Event EventName [Payload] | NoEvent deriving (Show, Eq)
data EventType = Private | Broadcast SessionID deriving (Show, Eq)
type Package = (EventType, Event)

instance Serializable Event where
    serialize = serialize . encode

instance FromJSON Event where
    parseJSON (Object v) =  Event <$>
                            v .: "name" <*>
                            v .: "args"
    parseJSON _ = return NoEvent

instance ToJSON Event where
   toJSON (Event name args) = object ["name" .= name, "args" .= args]
   toJSON NoEvent = object []