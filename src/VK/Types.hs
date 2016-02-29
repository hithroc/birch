module VK.Types where
import Config
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import Data.Aeson
import Data.Maybe
import Data.Time.Clock
import qualified Web.VKHS as V
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Monad.Trans.Control

type Dispatcher = String -> String -> String -> [(String, String)] -> IO (Either (Int, String) BS.ByteString)
data LongPollServer = LongPollServer { lpskey :: String, lpsurl :: String, lpsts :: Integer }
type MonadVK m = (MonadConfig m, MonadReader (TVar VKData) m, MonadIO m, MonadBaseControl IO m)

data ID = UserID { userID :: Integer } | ChatID { userID :: Integer, chatID :: Integer }
    deriving (Show, Ord)
data VKUser = VKUser ID String String
    deriving Show

data FriendRequests = FriendRequests [ID]

data VKData = VKData
    { accessToken :: String
    , expireDate :: Maybe UTCTime
    , accessRights :: [V.AccessRight]
    , dispatcher :: Dispatcher
    , apiVersion :: String
    , lastMessageID :: Integer
    , longPollServer :: Maybe LongPollServer
    , logUser :: VKUser
    , quoteCooldown :: Map.Map ID UTCTime
    }

data Message = Message
    { msgID :: Integer
    , uid :: ID
    , message :: String
    , attachment :: [String]
    , forwarded :: [Integer]
    }
    deriving Show
data MessageResponse = MessageResponse [Message]
    deriving Show

data LongPollValue
    = IntData  { intdata :: Integer }
    | TextData { textdata :: String }
    | ObjData  { objdata :: Map.Map String LongPollValue }
    deriving (Ord, Eq)

data LongPollResponse = LongPollResponse [[LongPollValue]] Integer
data ErrorResponse = ErrorResponse Int String

data PhotoServ = PhotoServ String
data UploadResponse = UploadResponse Integer String String
data DocUploadResponse = DocUploadResponse String
data DocResponse = DocResponse Integer Integer
data Photo = Photo Integer Integer
    deriving Show
data PhotoResponse = PhotoResponse Value
    deriving Show
data DocPersponse = DocResponce Int Int
    deriving Show

instance Eq ID where
    (==) a b = userID a == userID b


instance FromJSON LongPollServer where
    parseJSON (Object v) = do
        resp <- v .: "response"
        LongPollServer <$> resp .: "key"
                       <*> resp .: "server"
                       <*> resp .: "ts"
    parseJSON _ = mzero

instance FromJSON Message where
    parseJSON (Object v) = do
        mid <- v .: "id"
        userid <- v .: "user_id"
        chatid <- v .:? "chat_id"
        body <- v .: "body"
        return $ Message mid (maybe (UserID userid) (ChatID userid) chatid) body [] []
    parseJSON _ = mzero

instance FromJSON MessageResponse where
    parseJSON (Object v) = do
        resbody <- v .: "response"
        items <- resbody .: "items"
        return $ MessageResponse items
    parseJSON _ = mzero

instance FromJSON LongPollValue where
    parseJSON (Number n) = return $ IntData (truncate n)
    parseJSON (String t) = return $ TextData (T.unpack t) 
    parseJSON (Object v) = ObjData <$> parseJSON (Object v)
    parseJSON _ = mzero

instance FromJSON LongPollResponse where
    parseJSON (Object v) = LongPollResponse
                        <$> v .: "updates"
                        <*> v .: "ts"
    parseJSON _ = mzero

instance FromJSON PhotoServ where
    parseJSON (Object v) = do
        resbody <- v .: "response"
        url <- resbody .: "upload_url"
        return $ PhotoServ url
    parseJSON _ = mzero
instance FromJSON UploadResponse where
    parseJSON (Object v) = UploadResponse 
                        <$> v .: "server"
                        <*> v .: "photo"
                        <*> v .: "hash"
    parseJSON _ = mzero

instance FromJSON DocUploadResponse where
    parseJSON (Object v) = DocUploadResponse <$> v .: "file"
    parseJSON _ = mzero

instance FromJSON PhotoResponse where
    parseJSON (Object v) = do
        resbody <- v .: "response"
        return $ PhotoResponse resbody
    parseJSON _ = mzero

instance FromJSON DocResponse where
    parseJSON (Object v) = do
        res <- head <$> v .: "response"
        i <- res .: "id"
        oi <- res .: "owner_id"
        return $ DocResponse oi i
    parseJSON _ = mzero

instance FromJSON Photo where
    parseJSON (Object v) = Photo
                        <$> v .: "owner_id"
                        <*> v .: "id"
    parseJSON _ = mzero

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

instance FromJSON ErrorResponse where
    parseJSON (Object v) = do
        res <- v .: "error"
        code <- res .: "error_code"
        msg <- res .: "error_msg"
        return $ ErrorResponse code msg
    parseJSON _ = mzero

instance FromJSON FriendRequests where
    parseJSON (Object v) = do
        res <- v .: "response"
        items <- res .: "items"
        return . FriendRequests . map UserID $ items
    parseJSON _ = mzero
