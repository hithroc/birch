module Card.Json where

import Data.Aeson.Lens
import Data.Aeson
import Data.Maybe
import Data.Foldable
import qualified Network.Wreq as W
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import Control.Lens
import Config
import Card.Type
import qualified Data.ByteString.Lazy as BS
import Control.Exception as E
import qualified Network.HTTP.Client as H
import System.Log.Logger

getLatestVersion :: (MonadConfig m, MonadIO m) => m (Maybe String)
getLatestVersion = do
    url <- jsonURL <$> ask
    r <- liftIO . W.get $ url ++ "version.json"
    let o = decode $ r ^. W.responseBody :: Maybe Value
    return (o ^. key "version")

getVersion :: (MonadConfig m, MonadIO m) => m (Maybe String)
getVersion = do
    folder <- dataFol <$> ask
    o <- liftIO $ decode <$> BS.readFile (folder ++ "version.json")
    return (o ^. key "version")

downloadSets :: (MonadConfig m, MonadIO m) => m ()
downloadSets = do
    locs <- locales <$> ask
    liftIO $ infoM rootLoggerName ("Downloading card info for " ++ show locs)
    traverse_ downloadSet locs

downloadSet :: (MonadConfig m, MonadIO m) => String -> m ()
downloadSet loc = do
    url <- jsonURL <$> ask
    folder <- dataFol <$> ask
    r <- liftIO . W.get $ url ++ filename
    liftIO $ BS.writeFile (folder ++ filename) (r ^. W.responseBody)
    liftIO $ infoM rootLoggerName ("Downloaded " ++ filename)
    where
        filename = "AllSets." ++ loc ++ ".json"

updateSets :: (MonadConfig m, MonadIO m) => m ()
updateSets = do
    last' <- getLatestVersion
    ver' <- getVersion
    case zipMaybe last' ver' of
        Nothing -> liftIO $ warningM "main.cards" "Failed to fetch versions"
        Just (last, ver) -> when (ver /= last) $ do
                liftIO $ infoM rootLoggerName $ "The local cards version (" ++ ver  ++ ") in different from server's verion (" ++ last ++ ")"
                downloadSets
                url <- jsonURL <$> ask
                folder <- dataFol <$> ask
                r <- liftIO . W.get $ url ++ "version.json"
                liftIO $ BS.writeFile (folder ++ "version.json") (r ^. W.responseBody)
    where
        zipMaybe Nothing _ = Nothing
        zipMaybe _ Nothing = Nothing
        zipMaybe (Just a) (Just b) = Just (a,b)

readCards :: (MonadConfig m, MonadIO m) => String -> m [Card]
readCards locale = do
    folder <- dataFol <$> ask
    o <- liftIO $ decode <$> BS.readFile (folder ++ "AllSets." ++ locale ++ ".json")
    let c = fmap (mapMaybe (decode . encode)) (o ^. traverseObject :: Maybe [Value])
    return $ fromMaybe [] c
