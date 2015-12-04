{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Card.Json where

import Data.Aeson.Lens (key)
import Data.Aeson
import Network.Wreq
import Control.Monad.Reader
import Control.Lens
import Config
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
