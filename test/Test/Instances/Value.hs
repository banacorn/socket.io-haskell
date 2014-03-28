--------------------------------------------------------------------------------
-- | Value

{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instances.Value (Value(..), arbitraryJSONString) where

import              Web.SocketIO.Types.String              

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>))
import              Data.Aeson.Types
import qualified    Data.HashMap.Strict                     as H
import              Data.Scientific                         (Scientific, scientific)
import qualified    Data.Text.Lazy                          as TL
import              Data.Vector                             (fromList)
import              Test.QuickCheck

--------------------------------------------------------------------------------
-- Data.Aeson.Value instance of arbitrary    

instance Arbitrary StrictText where
    arbitrary = fmap fromString arbitrary

instance Arbitrary Text where
    arbitrary = fmap fromString arbitrary

instance Arbitrary Scientific where
    arbitrary = do
        c <- arbitrary
        e <- elements [0 .. 9]
        return (scientific c e)

instance Arbitrary Value where
    arbitrary = do
        o <- H.fromList . take 2 <$> listOf arbitraryPair
        a <- fromList . take 2 <$> listOf arbitrary
        s <- arbitrary
        n <- arbitrary
        b <- arbitrary
        elements [Object o, Array a, String s, Number n, Bool b]

        where   arbitraryPair = do
                    k <- arbitrary
                    v <- arbitrary
                    return (k, v)

arbitraryJSONString :: Gen Text
arbitraryJSONString = TL.filter badthings <$> arbitrary
    where   badthings '"' = False
            badthings '\\' = False
            badthings _ = True