{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

data Config = Config
    { vkLogin :: String
    , vkPass :: String
    , appID :: String
    , imageURL :: String
    }

instance FromJSON Config where
    parseJSON (Object v) = Config 
                        <$> v .: "login" 
                        <*> v .: "password"
                        <*> v .: "appID"
                        <*> v .: "imageURL"

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = decode <$> BS.readFile path
