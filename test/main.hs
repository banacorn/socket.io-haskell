module Main where

import Test.Framework (defaultMain)
import qualified Test.Protocol
import qualified Test.Simulator
import qualified Test.Unit

main :: IO ()
main = defaultMain
    [ Test.Protocol.test
    --, Test.Simulator.test
    , Test.Unit.test
    ]
