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
import Debug.Trace
import System.Log.Logger

downloadSet :: (MonadConfig m, MonadIO m) => m ()
downloadSet = do
    liftIO $ infoM rootLoggerName ("Downloading cards.json...")
    url <- jsonURL <$> get
    folder <- dataFol <$> get
    r <- liftIO . W.get $ url
    liftIO $ BS.writeFile (folder ++ "cards.json") (r ^. W.responseBody)
    liftIO $ infoM rootLoggerName ("Downloaded cards.json")

readCards :: (MonadConfig m, MonadIO m) => m [Card]
readCards = do
    folder <- dataFol <$> get
    (o :: Maybe [Value]) <- liftIO $ decode <$> BS.readFile (folder ++ "cards.json")
    let c :: [Card]
        c = fromMaybe [] $ fmap (mapMaybe (decode . encode)) o
    liftIO . infoM rootLoggerName $ "Loaded card database of total " ++ show (length c) ++ " cards"
    return c
