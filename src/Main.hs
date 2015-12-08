module Main where

import Config
import Card
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Ether.Implicit
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Char
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.IO
import qualified Data.Map as Map
import VK
import VK.Messages
import Control.Concurrent

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initLogger
    infoM rootLoggerName "Logging initiated"
    cfg <- loadConfig "config.json"
    case cfg of
        Nothing -> criticalM rootLoggerName "Failed to load config.json!"
        Just conf -> runReaderT main' conf

initLogger :: IO ()
initLogger = do
    let form = simpleLogFormatter "$time\t[$prio]\t$msg"
    fhand <- fileHandler "./vkbot.log" DEBUG
    shand <- streamHandler stderr WARNING
    let fhand' = setFormatter fhand form
        shand' = setFormatter shand form
    updateGlobalLogger rootLoggerName $ setHandlers [fhand', shand']
    updateGlobalLogger rootLoggerName $ setLevel DEBUG

main' :: (MonadConfig m, MonadIO m) => m ()
main' = do
    updateSets
    locs <- locales <$> ask
    cards <- traverse readCards locs
    let c = Map.fromList $ zip locs cards
    evalStateT (runReaderT init c) defaultVKData
    runReaderT loop (Map.fromList $ zip locs cards)
    where
        loop :: (MonadConfig m, MonadCardsDB m, MonadIO m) => m ()
        loop = do
            liftIO . putStrLn $ "Enter any text:"
            text <- liftIO getLine
            search <- searchBy cardID (head . map snd $ getCards text)
            liftIO . print $ search
            liftIO . putStr . unlines . map printCard . concat . Map.elems $ search
            loop
        init :: (MonadVK m, MonadCardsDB m) => m ()
        init = do
            login
            loop'
        loop' :: (MonadVK m, MonadCardsDB m) => m ()
        loop' = do
            liftIO $ threadDelay 1000000
            msgs <- getMessages
            traverse answer msgs
            liftIO $ print msgs
            loop'
        answer :: (MonadVK m, MonadCardsDB m) => Message -> m ()
        answer msg = do
            filtered <- traverse (searchBy name . snd) . getCards $ message msg
            let pure = map (concatMap snd . Map.toList) filtered
                text = unlines . map printCard . mapMaybe listToMaybe $ pure
            sendMessage (Message 0 (uid msg) text)
