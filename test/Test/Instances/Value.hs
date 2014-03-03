--------------------------------------------------------------------------------
-- | Value

{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instances.Value (Value(..)) where

import              Web.SocketIO.Types.String              

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>))
import              Data.Aeson.Types
import              Data.Aeson.Encode                       (encode)
import qualified    Data.HashMap.Strict                     as H
import              Data.Scientific                         (Scientific, scientific)
import              Data.Vector                             (fromList)
import              Test.QuickCheck

--------------------------------------------------------------------------------
-- Data.Aeson.Value instance of Serializable
instance Serializable Value where
    serialize = serialize . (encode :: Value -> LazyByteString)

--------------------------------------------------------------------------------
-- Data.Aeson.Value instance of arbitrary    

instance Arbitrary StrictText where
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
        elements [Object o, Array a, String s, Number n, Bool b, Null]

        where   arbitraryPair = do
                    k <- arbitrary
                    v <- arbitrary
                    return (k, v)