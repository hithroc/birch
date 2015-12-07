{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Config
import Card
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Control.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Char
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initLogger
    infoM rootLoggerName "Logging initiated"
    mcfg <- loadConfig "config.json"
    case mcfg of
        Nothing -> criticalM rootLoggerName "Failed to load config.json!"
        Just cfg ->  runReaderT main' cfg

initLogger :: IO ()
initLogger = do
    let form = simpleLogFormatter "$time\t[$prio]\t$msg"
    fhand <- fileHandler "./vkbot.log" DEBUG
    shand <- streamHandler stderr WARNING
    let fhand' = setFormatter fhand form
        shand' = setFormatter shand form
    updateGlobalLogger rootLoggerName $ setHandlers [fhand', shand']
    updateGlobalLogger rootLoggerName $ setLevel DEBUG

main' :: (MonadReader Config m, MonadIO m) => m ()
main' = do
    updateSets
    cards <- readCards "enUS"
    loop cards
    where
        loop cards = do
            liftIO . putStrLn $ "Enter any text:"
            text <- liftIO $ getLine
            let search = searchCards cards (map snd $ getCards text)
            liftIO . print $ search
            liftIO . putStr . unlines . map printCard $ search
            loop cards
