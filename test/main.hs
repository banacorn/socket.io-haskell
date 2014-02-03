module Main where

import Test.Framework (defaultMain)
import qualified Test.Protocol
import qualified Test.Request

main :: IO ()
main = defaultMain
    [ Test.Protocol.test
    , Test.Request.test
    ]
