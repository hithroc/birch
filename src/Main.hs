module Main where

import Config
import Card
import Data.Char
import Data.Maybe
import Data.Foldable
import qualified Data.Set as S
import Control.Monad.Trans
import Control.Monad.Ether.Implicit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple hiding (priority)
import System.IO
import qualified Data.Map as Map
import qualified Network.Wreq as W
import Control.Lens
import VK
import VK.Documents
import VK.Users
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import System.Exit
import Data.List
import Version
import Command
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans.Control


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initLogger
    infoM rootLoggerName "=== START ==="
    relaunch
    where
        handlers :: Int -> [Handler ()]
        handlers i = [E.Handler interruptHandler, E.Handler exitHandler, E.Handler $ mainHandler i]
        relaunch = main' `E.catches` handlers 5
        main' = do
            cfg <- loadConfig "config.json"
            let initCfg :: (MonadConfig m, MonadIO m, MonadBaseControl IO m) => m ()
                initCfg = do
                    cards <- readCards
                    tdata <- liftIO . atomically . newTVar $ defaultVKData
                    runReaderT (runReaderT initVK cards) tdata
                initVK :: (MonadVK m, MonadIO m, MonadCardsDB m) => m ()
                initVK = do
                    login
                    myUser <- getUser Nothing
                    case myUser of
                        Nothing -> liftIO $ warningM rootLoggerName "Failed to fetch user's name!"
                        Just myUser' -> do
                            tdata <- ask
                            liftIO . atomically . modifyTVar tdata $ (\x -> x { logUser = myUser' })
                    fork $ friendsLoop
                    loop
            case cfg of
                Nothing -> do
                    errorM rootLoggerName "Failed to load config.json!"
                    exitFailure
                Just cfg' -> do
                    tcfg <- atomically . newTVar $ cfg'
                    void $ runReaderT initCfg tcfg
        mainHandler :: Int -> SomeException -> IO ()
        mainHandler 0 e = do
            emergencyM rootLoggerName $ "It's dead, Jim"
            error "The program is completely crashed"
        mainHandler i e = do
            criticalM rootLoggerName $ "The program crashed with an exception: "
                                     ++ E.displayException e
                                     ++ "! Fix this!"
            infoM rootLoggerName $ "Recovering from crash"
            main' `E.catches` handlers (i-1)
        exitHandler :: ExitCode -> IO ()
        exitHandler (ExitFailure _) = exitFailure
        exitHandler _ = relaunch

        interruptHandler UserInterrupt = return ()
        interruptHandler _ = relaunch

initLogger :: IO ()
initLogger = do
    let form = simpleLogFormatter "$time [$prio]\t$msg"
    fhand <- fileHandler "./vkbot.log" DEBUG
    shand <- streamHandler stderr WARNING
    let fhand' = setFormatter fhand form
        shand' = setFormatter shand form
    updateGlobalLogger rootLoggerName $ setHandlers [fhand', shand']
    updateGlobalLogger rootLoggerName $ setLevel DEBUG

loop :: (MonadVK m, MonadCardsDB m) => m ()
loop = do
    msgs <- getLongPoll
    commands <- traverse (parseCommand) msgs
    traverse_ (\(x, y) -> fork $ execute (uid x) y) $ zip msgs commands
    loop

friendsLoop :: (MonadVK m, MonadCardsDB m) => m ()
friendsLoop = do
    ids <- getFriendRequests
    traverse_ acceptFriend ids
    liftIO $ threadDelay 5000000
    friendsLoop
    
