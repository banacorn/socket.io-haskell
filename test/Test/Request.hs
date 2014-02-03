{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Request where

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

--import Web.SocketIO.Types
--import Web.SocketIO.Request
import qualified Network.Wai as Wai

instance Show Wai.Request 
instance Arbitrary Wai.Request where
    arbitrary = return Wai.defaultRequest


samp :: IO ()
samp = sample (arbitrary :: Gen Wai.Request) 

--propProcessRequestInfoID :: Property
--propProcessRequestInfoID = property $ forAll arbitrary check
--    where   check msg = msg == msgIdentity msg
--            msgIdentity = parseMessage . serialize


test :: Test
test = undefined