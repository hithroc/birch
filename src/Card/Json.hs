{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Card.Json where

import Data.Aeson.Lens
import Data.Aeson
import Data.Maybe
import Network.Wreq
import Control.Monad.Reader
import Control.Lens
import Config
import Card.Type
import qualified Data.ByteString.Lazy as BS

getLatestVersion :: (MonadReader Config m, MonadIO m) => m (Maybe String)
getLatestVersion = do
    url <- jsonURL <$> ask
    r <- liftIO . get $ url ++ "version.json"
    let o = decode $ r ^. responseBody :: Maybe Value
    return (o ^. key "version")

getVersion :: (MonadReader Config m, MonadIO m) => m (Maybe String)
getVersion = do
    folder <- dataFol <$> ask
    o <- liftIO $ decode <$> (BS.readFile $ folder ++ "version.json")
    return (o ^. key "version")

readCards :: (MonadReader Config m, MonadIO m) => String -> m [Card]
readCards locale = do
    folder <- dataFol <$> ask
    o <- liftIO $ decode <$> (BS.readFile $ folder ++ "AllSets." ++ locale ++ ".json")
    let c = fmap (catMaybes . map (decode . encode)) (o ^. traverseObject :: Maybe [Value])
    return $ maybe [] id c
