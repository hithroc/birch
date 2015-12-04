{-# LANGUAGE OverloadedStrings #-}
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

searchByName :: String -> [Card] -> [Card]
searchByName n = filter (\c -> isInfixOf (map toUpper n) (map toUpper $ name c))

main :: IO ()
main = do
    (Just cfg) <- loadConfig "config.json"
    cards <- runReaderT (readCards "enUS") cfg
    n <- getLine
    putStr . unlines . map printCard . searchByName n $ cards
