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
newGlobalChannel :: MonadBase IO m => m (Chan Package)
newGlobalChannel = newChan

--------------------------------------------------------------------------------
newLogChannel :: MonadBase IO m => m (Chan ByteString)
newLogChannel = newChan

--------------------------------------------------------------------------------
streamToStdout :: Chan ByteString -> IO ()
streamToStdout channel = void . fork . forever $ do
    readChan channel >>= BC.putStrLn 

--------------------------------------------------------------------------------
makeChannelHub :: SessionID -> ConnectionM ChannelHub
makeChannelHub sessionID = do

    globalChannel <- envGlobalChannel <$> getEnv
    logChannel <- envLogChannel <$> getEnv

    -- duplicate global channel
    globalChannelClone <- dupChan globalChannel

    localChannel <- newChan
    outputChannel <- newChan

    streamBothChannelTo sessionID localChannel globalChannelClone outputChannel

    return $ ChannelHub localChannel globalChannelClone outputChannel logChannel

------------------------------------------------------------------------------
streamBothChannelTo :: (MonadBaseControl IO m, MonadBase IO m) => SessionID -> Chan Package -> Chan Package -> Chan Package -> m ()
streamBothChannelTo sessionID local global output = do
    -- local
    void . fork . forever $ readChan local >>= writeChan output
    -- global, drops Broacast package if same sessionID (same origin)
    void . fork . forever $ do
        package <- readChan global
        case package of
            (Private, _) -> writeChan output package
            (Broadcast sessionID', _) -> do
                if sessionID /= sessionID'
                    then writeChan output package
                    else return ()