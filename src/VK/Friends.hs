module VK.Friends where

import VK.Types
import VK.Base
import Data.Aeson
import Control.Monad.Trans
import Control.Monad
import System.Log.Logger

getFriendRequests :: MonadVK m => m ([ID])
getFriendRequests = do
    r <- dispatch "friends.getRequests" []
    case decode r of
        Nothing -> do
            liftIO $ warningM rootLoggerName "Failed to decode the response about friend requests!"
            return []
        Just (FriendRequests ids) -> return ids

acceptFriend :: MonadVK m => ID -> m ()
acceptFriend (ChatID _ _) = return ()
acceptFriend (UserID i) = void $ dispatch "friends.add" [("user_id", show i)]
