--------------------------------------------------------------------------------
-- | Event data types
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Event
    (   Event(..)
    ,   EventName
    ,   Payload_(..)
    ,   EventType(..)
    ,   Package
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types.String

----------------------------------------------------------------------------------
import              Control.Applicative
import              Data.Aeson                      as Aeson
import qualified    Data.Text.Lazy                  as TL
import              Data.Vector                     (toList)
--------------------------------------------------------------------------------
-- | Name of an Event
type EventName = Text

--------------------------------------------------------------------------------
-- | Payload carried by an Event
data Payload_ = Payload_ [Aeson.Value] deriving (Eq, Show)

instance Serializable Payload_ where
    serialize (Payload_ payload) = serialize $ Aeson.encode payload

--------------------------------------------------------------------------------
-- | Event
data Event = Event EventName Payload_
           | NoEvent                    -- ^ some malformed shit
           deriving (Show, Eq)

instance Serializable Event where
   serialize (Event name (Payload_ [])) = serialize $ "{\"name\":\"" `TL.append` name `TL.append` "\"}"
   serialize (Event name payload) = serialize $ "{\"name\":\"" `TL.append` name `TL.append` "\",\"args\":" `TL.append` serialize payload `TL.append` "}"
   serialize NoEvent = ""

instance FromJSON Event where
   parseJSON (Object v) = Event <$>
                          v .: "name" <*>
                          (toArgumentList <$> v .:? "args")
        where   toArgumentList :: Maybe Value -> Payload_
                toArgumentList Nothing          = Payload_ []
                toArgumentList (Just (Array a)) = Payload_ $ filter (/= Aeson.Null) . toList $ a
                toArgumentList _                = Payload_ []

   parseJSON _ = return NoEvent

instance ToJSON Event where
  toJSON (Event name (Payload_ []))      = object ["name" .= name]
  toJSON (Event name (Payload_ payload)) = object ["name" .= name, "args" .= payload]
  toJSON NoEvent = object []

--------------------------------------------------------------------------------
-- | For internal use only, indicates how Events are be triggered.
data EventType = Private                -- ^ `Web.SocketIO.Types.Base.emit` 
               | Broadcast ByteString   -- ^ `Web.SocketIO.Types.Base.broadcast`, with `SessionID` of the sender.
               deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Event packed with EventType
type Package = (EventType, Event)