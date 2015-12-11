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
import System.Log.Logger

getLatestVersion :: (MonadConfig m, MonadIO m) => m (Maybe String)
getLatestVersion = do
    url <- jsonURL <$> get
    r <- liftIO . W.get $ url ++ "version.json"
    let o = decode $ r ^. W.responseBody :: Maybe Value
    return (o ^. key "version")

getVersion :: (MonadConfig m, MonadIO m) => m (Maybe String)
getVersion = do
    folder <- dataFol <$> get
    o <- liftIO $ decode <$> BS.readFile (folder ++ "version.json")
    return (o ^. key "version")

downloadSets :: (MonadConfig m, MonadIO m) => m ()
downloadSets = do
    locs <- locales <$> get
    liftIO $ infoM rootLoggerName ("Downloading card info for " ++ show locs)
    traverse_ downloadSet locs

downloadSet :: (MonadConfig m, MonadIO m) => String -> m ()
downloadSet loc = do
    url <- jsonURL <$> get
    folder <- dataFol <$> get
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
        Just (l, v) -> when (v /= l) $ do
                liftIO $ infoM rootLoggerName $ "The local cards version (" ++ v  ++ ") in different from server's verion (" ++ l ++ ")"
                downloadSets
                url <- jsonURL <$> get
                folder <- dataFol <$> get
                r <- liftIO . W.get $ url ++ "version.json"
                liftIO $ BS.writeFile (folder ++ "version.json") (r ^. W.responseBody)
    where
        zipMaybe Nothing _ = Nothing
        zipMaybe _ Nothing = Nothing
        zipMaybe (Just a) (Just b) = Just (a,b)

readCards :: (MonadConfig m, MonadIO m) => Locale -> m [Card]
readCards (Locale loc) = do
    folder <- dataFol <$> get
    o <- liftIO $ decode <$> BS.readFile (folder ++ "AllSets." ++ loc ++ ".json")
    let c = fmap (mapMaybe (decode . encode)) (o ^. traverseObject :: Maybe [Value])
    return $ map (\x -> x { locale = Locale loc }) $ fromMaybe [] c
readCards Unknown = readCards $ Locale "enUS"
