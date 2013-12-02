{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.SocketIO.Types.String (
        S.IsString(..)
    ,   IsByteString(..)
    ,   IsLazyByteString(..)
    ,   IsText(..)
    ,   Text
    ,   (<>)
    ) where

import qualified Data.String as S
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.Monoid ((<>))

type Text = TL.Text

class IsByteString a where
    fromByteString :: B.ByteString -> a

instance IsByteString String where
    fromByteString = C.unpack

instance IsByteString TL.Text where
    fromByteString = TL.pack . fromByteString

instance IsByteString BL.ByteString where
    fromByteString = BL.fromStrict

class IsLazyByteString a where
    fromLazyByteString :: BL.ByteString -> a

instance IsLazyByteString String where
    fromLazyByteString = fromByteString . BL.toStrict

instance IsLazyByteString TL.Text where
    fromLazyByteString = fromByteString . BL.toStrict

instance IsLazyByteString B.ByteString where
    fromLazyByteString = BL.toStrict

class IsText a where
    fromText :: TL.Text -> a

instance IsText String where
    fromText = TL.unpack

instance IsText B.ByteString where
    fromText = C.pack . fromText

instance IsText BL.ByteString where
    fromText = BL.fromStrict . C.pack . fromText