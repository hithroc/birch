{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Config
import Card.Parser
import Card.Json
import Card.Type
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

searchByName :: String -> [Card] -> [Card]
searchByName n = filter (\c -> isInfixOf (map toUpper n) (map toUpper $ name c))

main :: IO ()
main = do
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
    undefined
