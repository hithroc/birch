module Main where

import Config
import Card hiding (name)
import Data.Foldable
import Control.Monad.Trans
import Control.Monad.Ether.Implicit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple hiding (priority)
import VK
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.STM
import Version
import Command
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans.Control

import System.Directory
import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon, fatalError)
import System.Posix.User

main :: IO ()
main = (serviced $ simpleDaemon { privilegedAction = rootInit, program = main' })

rootInit :: IO ()
rootInit = do
    -- Initialize logger
    let form = simpleLogFormatter "$time [$prio]\t$msg"
    fhand <- fileHandler "/var/log/birch.log" DEBUG
    updateGlobalLogger rootLoggerName $ setHandlers [setFormatter fhand form]
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    infoM rootLoggerName $ "Birch Daemon ver. " ++ version



loadConfigs :: IO (Maybe Config)
loadConfigs = do
    home <- homeDirectory <$> (getEffectiveUserID >>= getUserEntryForID)
    let dirs = [ home ++ "/.birch"
               , "/etc/birch.cfg" -- Upgrade directory package later
               ]
    x <- (traverse loadConfig dirs)
    return (join . listToMaybe . dropWhile isNothing $ x)

main' :: () -> IO ()
main' _ = do
    -- Read config
    cfg <- loadConfigs
    case cfg of
        Nothing -> do
            let logmsg = "Failed to load config file!"
            criticalM rootLoggerName logmsg
            fatalError logmsg
        Just cfg' -> do
            infoM rootLoggerName "Initialization finished"
            createDirectoryIfMissing True $ dataFol cfg'
            tcfg <- atomically . newTVar $ cfg'
            void $ runReaderT initCfg tcfg
    where
        initCfg :: (MonadConfig m, MonadIO m, MonadBaseControl IO m) => m ()
        initCfg = do
            cards <- readCards
            tdata <- liftIO . atomically . newTVar $ defaultVKData
            runReaderT (runReaderT initVK cards) tdata
        initVK :: (MonadVK m, MonadIO m, MonadCardsDB m) => m ()
        initVK = do
            login
            tcfg <- ask
            cfg <- liftIO . atomically . readTVar $ tcfg
            myUser <- getUser Nothing
            case myUser of
                Nothing -> do
                    let logmsg = "Failed to fetch user's name!"
                    liftIO $ criticalM rootLoggerName logmsg
                    liftIO $ fatalError logmsg
                Just myUser'@(VKUser _ fn ln) -> do
                    liftIO $ infoM rootLoggerName $ "User's name is " ++ fn ++ " " ++ ln
                    tdata <- ask
                    liftIO . atomically . modifyTVar tdata $ (\x -> x { logUser = myUser' })
            fork $ when (autoAccept cfg) $ do
                liftIO $ infoM rootLoggerName "Autoaccepting friends"
                friendsLoop
            liftIO $ infoM rootLoggerName "Main loop started"
            loop

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
