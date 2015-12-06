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
import Control.Exception as E
import qualified Network.HTTP.Client as H
import System.Log.Logger

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

downloadSets :: (MonadReader Config m, MonadIO m) => m ()
downloadSets = do
    locs <- locales <$> ask
    liftIO $ infoM rootLoggerName ("Downloading card info for " ++ show locs)
    sequence_ $ map downloadSet locs

downloadSet :: (MonadReader Config m, MonadIO m) => String -> m ()
downloadSet loc = do
    url <- jsonURL <$> ask
    folder <- dataFol <$> ask
    r <- liftIO . get $ url ++ filename
    liftIO $ BS.writeFile (folder ++ filename) (r ^. responseBody)
    liftIO $ infoM rootLoggerName ("Downloaded " ++ filename)
    where
        filename = "AllSets." ++ loc ++ ".json"

updateSets :: (MonadReader Config m, MonadIO m) => m ()
updateSets = do
    last' <- getLatestVersion
    ver' <- getVersion
    case zipMaybe last' ver' of
        Nothing -> do
            liftIO $ warningM "main.cards" "Failed to fetch versions"
        Just (ver, last) -> do
            if ver /= last  then do
                liftIO $ infoM rootLoggerName $ "The local cards version (" ++ ver  ++ ") in different from server's verion (" ++ last ++ ")"
                downloadSets
            else
                return ()
    where
        zipMaybe Nothing _ = Nothing
        zipMaybe _ Nothing = Nothing
        zipMaybe (Just a) (Just b) = Just (a,b)

readCards :: (MonadReader Config m, MonadIO m) => String -> m [Card]
readCards locale = do
    folder <- dataFol <$> ask
    o <- liftIO $ decode <$> (BS.readFile $ folder ++ "AllSets." ++ locale ++ ".json")
    let c = fmap (catMaybes . map (decode . encode)) (o ^. traverseObject :: Maybe [Value])
    return $ maybe [] id c
