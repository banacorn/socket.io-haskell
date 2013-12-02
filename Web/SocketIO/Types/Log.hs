module Web.SocketIO.Types.Log (Log(..)) where

import System.Console.ANSI

-- Logger
data Log = Error String
         | Warn String
         | Info String
         | Debug String
         deriving (Eq)

instance Show Log where
    show (Error message) = "    " ++ paint Red 	"[error] " ++ error message
    show (Warn  message) = "    " ++ paint Yellow "[warn]  " ++ message
    show (Info  message) = "    " ++ paint Cyan   "[info]  " ++ message
    show (Debug message) = "    " ++ paint Black  "[debug] " ++ message

paint color s = setSGRCode [SetColor Foreground Vivid color] ++ s ++ setSGRCode []
