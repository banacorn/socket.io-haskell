--------------------------------------------------------------------------------
-- | Protocol test

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Protocol (test) where

import              Web.SocketIO.Types
import              Web.SocketIO.Protocol

import              Data.Aeson
import              Test.Instances.Value
--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>))
import qualified    Data.Text.Lazy                          as TL
import              Test.Framework
import              Test.QuickCheck
import              Test.Framework.Providers.QuickCheck2

--------------------------------------------------------------------------------
-- make data types of protocol instance of arbitrary    

friendlyStringGen :: IsLazyText a => Gen a
friendlyStringGen = fmap (fromLazyText . TL.cons 'a' . TL.replace ":" ".") (arbitrary)

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

instance Arbitrary Payload where
    arbitrary = do
        payload <- listOf (arbitrary :: Gen Value)
        return $ Payload payload

instance Arbitrary Event where
    arbitrary = do
        eventName <- arbitraryJSONString
        payload <- arbitrary
        elements [NoEvent, Event eventName payload]

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

instance Arbitrary a => Arbitrary (Framed a) where
    arbitrary = Framed <$> (listOf arbitrary)

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

propParseFramedMessageID :: Property
propParseFramedMessageID = property $ forAll arbitrary check
    where   check msg = msg == msgIdentity msg
            msgIdentity msg = parseFramedMessage $ serialize msg

propParsePathID :: Property
propParsePathID = property $ forAll arbitrary check
    where   check path = path == pathIdentity path
            pathIdentity = parsePath . serialize

test :: Test
test = testGroup "Protocol"
    [ testProperty "parseFramedMessage" propParseFramedMessageID
    , testProperty "parsePath"    propParsePathID
    ]