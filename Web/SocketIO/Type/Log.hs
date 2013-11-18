module Web.SocketIO.Type.Log (Log(..), Show(..)) where


import System.Console.ANSI

-- Logger
data Log = Error String
         | Warn String
         | Info String
         | Debug String
         deriving (Eq)

instance Show Log where
    show (Error message) = error message
    show (Warn  message) = paint Yellow "[warn]  " ++ message
    show (Info  message) = paint Blue   "[info]  " ++ message
    show (Debug message) = paint Green  "[debug] " ++ message

paint color s = setSGRCode [SetColor Foreground Vivid color] ++ s ++ setSGRCode []
