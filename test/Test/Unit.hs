{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Unit where

import qualified    Test.Unit.Connection                    as Connection
import qualified    Test.Unit.Session                       as Session

import qualified    Test.Framework                          as Framework
import              Test.Framework

test :: Framework.Test
test = testGroup "Unit Tests" 
    [ Connection.test
    , Session.test
    ]