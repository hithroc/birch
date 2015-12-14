module VK.Users where

import VK.Base
import Data.Aeson
import Control.Monad
import Data.Maybe

instance FromJSON VKUser where
    parseJSON (Object v) = do
        res <- v .: "response"
        case listToMaybe res of
            Nothing -> mzero
            Just o -> do
                userid <- o .: "id"
                firstname <- o .: "first_name"
                lastname <- o .: "last_name"
                return $ VKUser (UserID userid) firstname lastname
    parseJSON _ = mzero

getUser :: MonadVK m => Maybe ID -> m (Maybe VKUser)
getUser i = do
    let args = case i of
            Nothing -> []
            Just i' -> [("user_ids", show $ userID i')]

    decode <$> dispatch "users.get" args
