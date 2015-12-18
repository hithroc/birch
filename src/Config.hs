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
    , soundURL :: String
    , jsonURL :: String
    , dataFol :: String
    , locales :: [String]
    , aliases :: Aliases
    , admins :: [Integer]
    , bannedForQuote :: [Integer]
    }

instance FromJSON Config where
    parseJSON (Object v) = Config 
                        <$> v .: "login" 
                        <*> v .: "password"
                        <*> v .: "appID"
                        <*> v .: "imageURL"
                        <*> v .: "soundURL"
                        <*> v .: "jsonURL"
                        <*> v .: "data"
                        <*> v .: "locales"
                        <*> (Map.mapKeys (map toUpper) <$> v .: "aliases")
                        <*> v .: "admins"
                        <*> v .: "quotebanned"
    parseJSON _ = mzero

type MonadConfig = MonadState Config

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = decode <$> BS.readFile path
