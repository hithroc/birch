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
    evalStateT loop' defaultVKData
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
        loop' :: MonadVK m => m ()
        loop' = do
            threadDelay 1000000
            msgs <- getMessages
            print msgs
