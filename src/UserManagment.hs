module UserManagment where

import Data.Aeson
import Control.Monad
import qualified Data.Map as Map

data UserPermissions = UserPermissions { userDB :: Map.Map Integer Permission }

data Permission
    = Admin
    | Honored
    | User
    | Banned
    | Everyone
    deriving (Eq, Ord, Show, Read)

permMsgs = Map.fromList $
    [(Banned, "Sorry, but you are banned. You probably deserve it though.")]

instance FromJSON UserPermissions where
    parseJSON (Object v) = do
        admins <- v .: "admins"
        honored <- v .: "honored"
        banned <- v .: "banned"
        return . UserPermissions . Map.fromList $ zip admins (repeat Admin) ++ zip honored (repeat Honored) ++ zip banned (repeat Banned)
    parseJSON _ = mzero
