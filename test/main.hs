module Main where

import Test.Framework (defaultMain)
import qualified Test.Protocol

main :: IO ()
main = defaultMain
    [ Test.Protocol.test
    ]
