module VK.Users where

import VK.Base
import Data.Aeson
import Control.Monad
import Data.Maybe
import VK.Types


getUser :: MonadVK m => Maybe ID -> m (Maybe VKUser)
getUser i = do
    let args = case i of
            Nothing -> []
            Just i' -> [("user_ids", show $ userID i')]

    decode <$> dispatch "users.get" args
