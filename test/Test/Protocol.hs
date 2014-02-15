--------------------------------------------------------------------------------
-- | Protocol test

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Protocol (test) where

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Web.SocketIO.Types
import Web.SocketIO.Protocol

--------------------------------------------------------------------------------
-- make data types of protocol instance of arbitrary    

friendlyStringGen :: IsString a => Gen a
friendlyStringGen = fmap (fromString . map replaceColon) (listOf1 arbitrary)
    where replaceColon c = if c == ':' then '.' else c

instance Arbitrary Endpoint where
    arbitrary = do
        endpoint <- friendlyStringGen
        elements [NoEndpoint, Endpoint endpoint]

instance Arbitrary ID where
    arbitrary = fmap (ID . abs) arbitrary

instance Arbitrary Data where
    arbitrary = do
        d <- friendlyStringGen
        elements [NoData, Data d]

instance Arbitrary Text where
    arbitrary = fmap fromString arbitrary

instance Arbitrary Event where
    arbitrary = do
        eventName <- arbitrary
        payloads <- listOf arbitrary
        elements [NoEvent, Event eventName payloads]

instance Arbitrary Message where
    arbitrary = do
        i <- arbitrary
        d <- arbitrary
        endpoint <- arbitrary
        emitter <- arbitrary
        elements
            [ MsgDisconnect endpoint
            , MsgConnect endpoint
            , MsgHeartbeat
            , Msg i endpoint d
            , MsgJSON i endpoint d
            , MsgEvent i endpoint emitter
            , MsgACK i d
            , MsgError endpoint d
            , MsgNoop
            ]

--------------------------------------------------------------------------------
-- make path instance of arbitrary    

arbitraryPath :: Gen ByteString
arbitraryPath = fmap (fromString . map replaceSlash) (listOf1 arbitrary)
    where replaceSlash c = if c == '/' then '_' else c
    
instance Arbitrary Transport where
    arbitrary = elements [WebSocket, XHRPolling, NoTransport]

instance Arbitrary Path where
    arbitrary = do
        namespace <- arbitraryPath
        protocol <- arbitraryPath
        transport <- arbitrary
        sessionID <- arbitraryPath
        elements
            [ WithSession namespace protocol transport sessionID
            , WithoutSession namespace protocol
            ]

--------------------------------------------------------------------------------
-- properties

propParseMessageID :: Property
propParseMessageID = property $ forAll arbitrary check
    where   check msg = msg == msgIdentity msg
            msgIdentity = parseMessage . serialize

propParsePathID :: Property
propParsePathID = property $ forAll arbitrary check
    where   check path = path == pathIdentity path
            pathIdentity = parsePath . serialize

test :: Test
test = testGroup "Protocol"
    [ testProperty "parseMessage" propParseMessageID
    , testProperty "parsePath"    propParsePathID
    ] 
