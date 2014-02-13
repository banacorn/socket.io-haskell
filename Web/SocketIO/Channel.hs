{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.SocketIO.Channel 
    (   newGlobalChannel
    ,   newLogChannel
    ,   streamToStdout
    ,   streamBothChannelTo
    ,   makeChannelHub
    ) where

--------------------------------------------------------------------------------
import              Web.SocketIO.Types

--------------------------------------------------------------------------------
import              Control.Applicative                     ((<$>))
import              Control.Concurrent.Chan.Lifted 
import              Control.Concurrent.Lifted               (fork)
import              Control.Monad                           (forever, void)
import              Control.Monad.Base                      (MonadBase)
import              Control.Monad.Trans.Control             (MonadBaseControl)
import qualified    Data.ByteString.Char8                   as BC

--------------------------------------------------------------------------------
newGlobalChannel :: MonadBase IO m => m (Chan Event)
newGlobalChannel = newChan

--------------------------------------------------------------------------------
newLogChannel :: MonadBase IO m => m (Chan ByteString)
newLogChannel = newChan

--------------------------------------------------------------------------------
streamToStdout :: Chan ByteString -> IO ()
streamToStdout channel = void . fork . forever $ do
    readChan channel >>= BC.putStrLn 

--------------------------------------------------------------------------------
makeChannelHub :: ConnectionM ChannelHub
makeChannelHub = do

    globalChannel <- envGlobalChannel <$> getEnv
    logChannel <- envLogChannel <$> getEnv

    -- duplicate global channel
    globalChannelClone <- dupChan globalChannel

    localChannel <- newChan
    outputChannel <- newChan

    streamBothChannelTo localChannel globalChannelClone outputChannel

    return $ ChannelHub localChannel globalChannelClone outputChannel logChannel

------------------------------------------------------------------------------
streamBothChannelTo :: (MonadBaseControl IO m, MonadBase IO m) => Chan a -> Chan a -> Chan a -> m ()
streamBothChannelTo a b c = do
    void . fork . forever $ readChan a >>= writeChan c
    void . fork . forever $ readChan b >>= writeChan c