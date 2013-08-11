{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketIO.Util (IsString(..), IsByteString(..), IsText(..)) where
import Data.String
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

class IsByteString a where
    fromByteString :: B.ByteString -> a

instance IsByteString String where
    fromByteString = C.unpack

instance IsByteString T.Text where
    fromByteString = T.pack . fromByteString

class IsText a where
    fromText :: T.Text -> a

instance IsText String where
    fromText = T.unpack

instance IsText B.ByteString where
    fromText = C.pack . fromText