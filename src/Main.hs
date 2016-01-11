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
import VK.Users
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import System.Exit
import Data.List
import Version
import Command

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
            let initCfg :: (MonadConfig m, MonadIO m) => m ()
                initCfg = do
                    updateSets
                    locs <- locales <$> get
                    cards <- traverse (readCards . Locale) locs
                    let c = Map.fromList $ zip locs cards
                    evalStateT (runReaderT initVK c) defaultVKData
                initVK :: (MonadVK m, MonadIO m, MonadCardsDB m) => m ()
                initVK = do
                    login
                    myUser <- getUser Nothing
                    case myUser of
                        Nothing -> liftIO $ warningM rootLoggerName "Failed to fetch user's name!"
                        Just myUser' -> modify (\x -> x { logUser = myUser' })
                    loop
            case cfg of
                Nothing -> do
                    errorM rootLoggerName "Failed to load config.json!"
                    exitFailure
                Just cfg' -> void $ evalStateT initCfg cfg'
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
    (VKUser _ uname _) <- logUser <$> get
    msgs <- getLongPoll
    commands <- traverse (parseCommand) msgs
    cfg <- get
    vkdata <- get
    cards <- ask
    tcfg <- liftIO . atomically $ newTVar cfg
    tvkdata <- liftIO . atomically $ newTVar vkdata
    traverse_ (\(x, y) -> liftIO . void . forkIO $ executeIO tvkdata tcfg cards (uid x) y) $ zip msgs commands
    cfg' <- liftIO . atomically $ readTVar tcfg
    vkdata' <- liftIO . atomically $ readTVar tvkdata
    modify (const cfg')
    modify (const vkdata')
    loop
