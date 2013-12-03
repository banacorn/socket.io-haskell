{-# LANGUAGE OverloadedStrings #-}

module Web.SocketIO.Event where

import Web.SocketIO.Types
import Web.SocketIO.Session

import Control.Monad.Writer
import Control.Monad.Reader
import Data.IORef.Lifted

reply :: CallbackM Reply
reply = ask
