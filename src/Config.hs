module Config where

import Data.Aeson
import Data.Time.Clock
import Data.Char
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Ether.Implicit
import qualified Data.Map as Map
import UserManagment
import Control.Concurrent.STM

type Aliases = Map.Map String String

data Config = Config
    { vkLogin :: String
    , vkPass :: String
    , appID :: String
    , imageURL :: String
    , soundURL :: String
    , jsonURL :: String
    , dataFol :: String
    , aliases :: Aliases
    , userperm :: UserPermissions
    , bannedForQuote :: [Integer]
    , quoteCD :: NominalDiffTime
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
                        <*> (Map.mapKeys (map toUpper) <$> v .: "aliases")
                        <*> v .: "permissions"
                        <*> v .: "quotebanned"
                        <*> (fromInteger <$> v .: "quotecd")
    parseJSON _ = mzero

type MonadConfig = MonadReader (TVar Config)

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = decode <$> BS.readFile path
