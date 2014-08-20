--------------------------------------------------------------------------------
-- | Parser
{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Parser 
    (   parse
    ,   module Web.SocketIO.Parser.URL
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types
import              Web.SocketIO.Parser.URL


--------------------------------------------------------------------------------
import              Data.Attoparsec.ByteString  (Parser, parseOnly)


--------------------------------------------------------------------------------
-- | wrapped Attoparsec parser
parse :: Parser a -> ByteString -> a
parse parser input = case parseOnly parser input of
    Left  err    -> error (show err)
    Right result -> result