--------------------------------------------------------------------------------
-- | Event data types
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
-- | Name of an Event
type EventName = Text

--------------------------------------------------------------------------------
-- | Payload carried by an Event
type Payload = Text

--------------------------------------------------------------------------------
-- | Event
data Event = Event EventName [Payload] 
           | NoEvent                    -- ^ some malformed shit
           deriving (Show, Eq)

instance Serializable Event where
   serialize = serialize . encode

instance FromJSON Event where
   parseJSON (Object v) =  Event <$>
                           v .: "name" <*>
                           v .:? "args" .!= []
   parseJSON _ = return NoEvent

instance ToJSON Event where
  toJSON (Event name [])   = object ["name" .= name]
  toJSON (Event name args) = object ["name" .= name, "args" .= args]
  toJSON NoEvent = object []

--------------------------------------------------------------------------------
-- | For internal use only, indicates how Events are be triggered.
data EventType = Private                -- ^ `Web.SocketIO.Types.Base.emit` 
               | Broadcast ByteString   -- ^ `Web.SocketIO.Types.Base.broadcast`, with `SessionID` of the sender.
               deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Event packed with EventType
type Package = (EventType, Event)