module SocketIO.Message.Type where


data State = Connecting | Connected | Disconnecting | Disconnected deriving Show



data Message = Message {
    messageType :: MessageType,
    messageID :: MessageID,
    messageEndpoint :: MessageEndpoint,
    messageData :: MessageData
}

instance Show Message where
    show (Message t i e MessageDataAbsent) = show t ++ ":" ++ show i ++ ":" ++ show e
    show (Message t i e d) = show t ++ ":" ++ show i ++ ":" ++ show e ++ ":" ++ show d

data MessageType    = Disconnect 
                    | Connect 
                    | Heartbeat 
                    | RegularMessage
                    | JSONMessage
                    | Event
                    | Ack
                    | Error
                    | Noop
                    deriving (Enum)

instance Show MessageType where
    show = show . fromEnum

data MessageID = Omitted | MessageID Int | MessageIDPlus Int

instance Show MessageID where
    show Omitted = "" 
    show (MessageID i) = show i
    show (MessageIDPlus i) = show i ++ "+"

data MessageEndpoint = MessageEndpoint String

instance Show MessageEndpoint where
    show (MessageEndpoint s) = s

data MessageData    = MessageData String
                    | MessageDataAbsent

instance Show MessageData where
    show (MessageData s) = s
    show MessageDataAbsent = ""