{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instances where
--------------------------------------------------------------------------------
import              Test.QuickCheck
import              Control.Applicative                     ((<$>))

import              Web.SocketIO.Types
--------------------------------------------------------------------------------
data Operation = OpConnect 
               | OpDisconnect 
               | OpEmit Event
               deriving Show
data Scheme = Scheme [EventName] [Operation] deriving Show

instance Arbitrary SessionID where
    arbitrary = fromString <$> vectorOf 20 (elements alphaNum)
        where   alphaNum = ['A' .. 'Z']
                        ++ ['a' .. 'z']
                        ++ ['0' .. '9']


instance Arbitrary EventName where
    arbitrary = fromString . (:) '#' <$> vectorOf 4 (elements ['a' .. 'z'])

arbitraryPayload :: Gen Payload
arbitraryPayload = fromString . (:) ':' <$> vectorOf 10 (elements ['a' .. 'z'])

arbitraryEvent :: [EventName] -> Gen Event
arbitraryEvent [] = do
        payloads <- listOf arbitraryPayload
        return (Event "#####" payloads)
arbitraryEvent eventNames = do
        event <- elements eventNames
        payloads <- listOf arbitraryPayload
        return (Event event payloads)

arbitraryOperation :: [EventName] -> Gen Operation
arbitraryOperation eventNames = do
    event <- arbitraryEvent eventNames
    frequency
        [ (1, elements [OpConnect, OpDisconnect])
        , (2, elements [OpEmit event])
        ]

instance Arbitrary Scheme where
    arbitrary = oneof [wellformed]
        where   wellformed = do
                    eventNames <- arbitrary
                    operations <- map OpEmit <$> listOf (arbitraryEvent eventNames)
                    return (Scheme eventNames ([OpConnect] ++ operations ++ [OpDisconnect]))
                --illformed = do
                --    eventNames <- arbitrary
                --    operations <- listOf (arbitraryOperation eventNames)
                --    return (Scheme eventNames operations)

instance Arbitrary (HandlerM a) where
    arbitrary = undefined

--------------------------------------------------------------------------------
-- | CallbackEnv

--arbitraryCallbackEnv :: ChannelHub -> SessionID -> Gen CallbackEnv
--arbitraryCallbackEnv hub sessionID = do
--    payloads <- listOf arbitraryPayload
--    return $ CallbackEnv payloads hub sessionID

--------------------------------------------------------------------------------
-- | CallbackM

--arbitraryCallbackM :: [Event] -> ChannelHub -> SessionID -> Gen CallbackEnv
--arbitraryCallbackM hub sessionID = do
--    payloads <- listOf arbitraryPayload
--    return $ CallbackEnv payloads hub sessionID




--t :: IO ()
--t = sample arbitraryPayload

--s :: IO ()
--s = sample (arbitrary :: Gen (Payload))