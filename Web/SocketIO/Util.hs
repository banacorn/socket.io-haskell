{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Util ((<>), IsString(..), IsByteString(..), IsLazyByteString(..), IsText(..), debug) where

import Data.Monoid ((<>))
import Data.String
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans (liftIO, MonadIO)

debug :: (MonadIO m, Monad m) => String -> m ()
--debug = liftIO . print
debug _ = return ()
class IsByteString a where
    fromByteString :: B.ByteString -> a

instance IsByteString String where
    fromByteString = C.unpack

instance IsByteString T.Text where
    fromByteString = T.pack . fromByteString

instance IsByteString LB.ByteString where
    fromByteString = LB.fromStrict

class IsLazyByteString a where
    fromLazyByteString :: LB.ByteString -> a

instance IsLazyByteString String where
    fromLazyByteString = fromByteString . LB.toStrict

instance IsLazyByteString T.Text where
    fromLazyByteString = fromByteString . LB.toStrict

instance IsLazyByteString B.ByteString where
    fromLazyByteString = LB.toStrict

class IsText a where
    fromText :: T.Text -> a

instance IsText String where
    fromText = T.unpack

instance IsText B.ByteString where
    fromText = C.pack . fromText

instance IsText LB.ByteString where
    fromText = LB.fromStrict . C.pack . fromText