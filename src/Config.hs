module Config where

import Data.Aeson
import Data.Char
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Ether.Implicit
import qualified Data.Map as Map

type Aliases = Map.Map String String

data Config = Config
    { vkLogin :: String
    , vkPass :: String
    , appID :: String
    , imageURL :: String
    , jsonURL :: String
    , dataFol :: String
    , locales :: [String]
    , aliases :: Aliases
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
                        <*> (Map.mapKeys (map toUpper) <$> v .: "aliases")
    parseJSON _ = mzero

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = decode <$> BS.readFile path
