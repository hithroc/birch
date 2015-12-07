module Config where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Ether.Implicit

data Config = Config
    { vkLogin :: String
    , vkPass :: String
    , appID :: String
    , imageURL :: String
    , jsonURL :: String
    , dataFol :: String
    , locales :: [String]
    }

type MonadConfig = MonadReader Config

instance FromJSON Config where
    parseJSON (Object v) = Config 
                        <$> v .: "login" 
                        <*> v .: "password"
                        <*> v .: "appID"
                        <*> v .: "imageURL"
                        <*> v .: "jsonURL"
                        <*> v .: "data"
                        <*> v .: "locales"

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = decode <$> BS.readFile path
