--------------------------------------------------------------------------------
-- | Data types for logging
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Types.Log (Log(..), Serializable(..)) where

--------------------------------------------------------------------------------
import System.Console.ANSI
import Web.SocketIO.Types.String

--------------------------------------------------------------------------------
-- | Logger
data Log = Error ByteString
         | Warn ByteString
         | Info ByteString
         | Debug ByteString
         deriving (Eq, Show)

instance Serializable Log where
    serialize (Error message) = fromString $ "    " ++ (paint Red    $ "[error] " ++ error (fromByteString message))
    serialize (Warn  message) = fromString $ "    " ++ (paint Yellow $ "[warn]  " ++ fromByteString message)
    serialize (Info  message) = fromString $ "    " ++ (paint Cyan   $ "[info]  " ++ fromByteString message)
    serialize (Debug message) = fromString $ "    " ++ (paint Black  $ "[debug] " ++ fromByteString message)

--------------------------------------------------------------------------------
-- | helper function
paint :: Color -> String -> String
paint color s = setSGRCode [SetColor Foreground Vivid color] ++ s ++ setSGRCode []
