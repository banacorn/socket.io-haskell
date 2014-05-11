{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instances where
--------------------------------------------------------------------------------
--import              Test.QuickCheck
--import              Test.Instances.Value
--import              Control.Applicative                     ((<$>))

--import              Web.SocketIO.Types
--------------------------------------------------------------------------------
--data Operation = OpConnect 
--               | OpDisconnect 
--               | OpEmit Event
--               deriving Show
--data Scheme = Scheme [EventName] [Operation] deriving Show

--instance Arbitrary SessionID where
--    arbitrary = fromString <$> vectorOf 20 (elements alphaNum)
--        where   alphaNum = ['A' .. 'Z']
--                        ++ ['a' .. 'z']
--                        ++ ['0' .. '9']


----instance Arbitrary EventName where
----    arbitrary = fromString . (:) '#' <$> vectorOf 4 (elements ['a' .. 'z'])

--arbitraryPayload :: Gen Payload
--arbitraryPayload = Payload <$> listOf arbitrary

--arbitraryEvent :: [EventName] -> Gen Event
--arbitraryEvent [] = do
--        payload <- arbitraryPayload
--        return (Event "#####" payload)
--arbitraryEvent eventNames = do
--        event <- elements eventNames
--        payload <- arbitraryPayload
--        return (Event event payload)

--arbitraryOperation :: [EventName] -> Gen Operation
--arbitraryOperation eventNames = do
--    event <- arbitraryEvent eventNames
--    frequency
--        [ (1, elements [OpConnect, OpDisconnect])
--        , (2, elements [OpEmit event])
--        ]

--instance Arbitrary Scheme where
--    arbitrary = oneof [wellformed]
--        where   wellformed = do
--                    eventNames <- arbitrary
--                    operations <- map OpEmit <$> listOf (arbitraryEvent eventNames)
--                    return (Scheme eventNames ([OpConnect] ++ operations ++ [OpDisconnect]))
--                --illformed = do
--                --    eventNames <- arbitrary
--                --    operations <- listOf (arbitraryOperation eventNames)
--                --    return (Scheme eventNames operations)

--instance Arbitrary (HandlerM a) where
--    arbitrary = undefined