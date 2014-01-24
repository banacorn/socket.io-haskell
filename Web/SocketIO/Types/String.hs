{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.SocketIO.Types.String (
        S.IsString(..)
    ,   IsByteString(..)
    ,   IsLazyByteString(..)
    ,   IsText(..)
    ,   IsLazyText(..)
    ,   Serializable(..)
    ,   Text
    ,   ByteString
    ,   LazyText
    ,   (<>)
    ) where

--------------------------------------------------------------------------------
import qualified    Data.String                             as S
import qualified    Data.Text                               as T
import qualified    Data.Text.Lazy                          as TL
import qualified    Data.ByteString                         as B
import qualified    Data.ByteString.Lazy                    as BL
import qualified    Data.ByteString.Char8                   as C
import              Data.Monoid                             ((<>), Monoid)

--------------------------------------------------------------------------------
type Text = T.Text
type LazyText = TL.Text
type ByteString = B.ByteString

--------------------------------------------------------------------------------
class IsByteString a where
    fromByteString :: ByteString -> a

--------------------------------------------------------------------------------
instance IsByteString String where
    fromByteString = C.unpack

--------------------------------------------------------------------------------
instance IsByteString T.Text where
    fromByteString = T.pack . fromByteString

--------------------------------------------------------------------------------
instance IsByteString TL.Text where
    fromByteString = TL.pack . fromByteString

--------------------------------------------------------------------------------
instance IsByteString ByteString where
    fromByteString = id

--------------------------------------------------------------------------------
instance IsByteString BL.ByteString where
    fromByteString = BL.fromStrict

--------------------------------------------------------------------------------
class IsLazyByteString a where
    fromLazyByteString :: BL.ByteString -> a

--------------------------------------------------------------------------------
instance IsLazyByteString String where
    fromLazyByteString = fromByteString . BL.toStrict

--------------------------------------------------------------------------------
instance IsLazyByteString T.Text where
    fromLazyByteString = fromByteString . BL.toStrict

--------------------------------------------------------------------------------
instance IsLazyByteString TL.Text where
    fromLazyByteString = fromByteString . BL.toStrict

--------------------------------------------------------------------------------
instance IsLazyByteString ByteString where
    fromLazyByteString = BL.toStrict

--------------------------------------------------------------------------------
instance IsLazyByteString BL.ByteString where
    fromLazyByteString = id

--------------------------------------------------------------------------------
class IsText a where
    fromText :: T.Text -> a

--------------------------------------------------------------------------------
instance IsText String where
    fromText = T.unpack

--------------------------------------------------------------------------------
instance IsText Text where
    fromText = id

--------------------------------------------------------------------------------
instance IsText TL.Text where
    fromText = TL.fromStrict

--------------------------------------------------------------------------------
instance IsText ByteString where
    fromText = C.pack . fromText

--------------------------------------------------------------------------------
instance IsText BL.ByteString where
    fromText = BL.fromStrict . C.pack . fromText

--------------------------------------------------------------------------------
class IsLazyText a where
    fromLazyText :: TL.Text -> a

--------------------------------------------------------------------------------
instance IsLazyText String where
    fromLazyText = TL.unpack

--------------------------------------------------------------------------------
instance IsLazyText T.Text where
    fromLazyText = TL.toStrict

--------------------------------------------------------------------------------
instance IsLazyText TL.Text where
    fromLazyText = id

--------------------------------------------------------------------------------
instance IsLazyText ByteString where
    fromLazyText = C.pack . fromLazyText

--------------------------------------------------------------------------------
instance IsLazyText BL.ByteString where
    fromLazyText = BL.fromStrict . C.pack . fromLazyText

--------------------------------------------------------------------------------
class Serializable a where
    serialize :: ( Monoid s
                 , S.IsString s
                 , IsText s
                 , IsLazyText s
                 , IsByteString s
                 , IsLazyByteString s
                 , Show a) => a -> s
    serialize = S.fromString . show

instance Serializable String where
    serialize = S.fromString

instance Serializable T.Text where
    serialize = fromText

instance Serializable TL.Text where
    serialize = fromLazyText

instance Serializable ByteString where
    serialize = fromByteString
    
instance Serializable BL.ByteString where
    serialize = fromLazyByteString