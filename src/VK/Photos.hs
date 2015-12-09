module VK.Photos where

import VK.Base
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSS
import Network.HTTP.Types.URI

data PhotoServ = PhotoServ String
data UploadResponse = UploadResponse Integer String String
data Photo = Photo Integer Integer
    deriving Show
data PhotoResponse = PhotoResponse Value
    deriving Show

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
instance FromJSON PhotoResponse where
    parseJSON (Object v) = do
        resbody <- v .: "response"
        return $ PhotoResponse resbody
    parseJSON _ = mzero
instance FromJSON Photo where
    parseJSON (Object v) = Photo
                        <$> v .: "owner_id"
                        <*> v .: "id"
    parseJSON _ = mzero

uploadPhoto :: MonadVK m => BS.ByteString -> m (String)
uploadPhoto raw = do
    serv <- dispatch "photos.getMessagesUploadServer" []
    case decode serv of
        Nothing -> return ""
        Just (PhotoServ servstr) -> do
            r <- liftIO $ W.post servstr [W.partLBS "photo" raw & W.partFileName .~ Just "img.png"]
            case decode (r ^. W.responseBody) of
                Nothing -> return ""
                Just (UploadResponse s p h) -> do
                    info <- dispatch "photos.saveMessagesPhoto" [("server", show s), ("photo", BSS.unpack . urlEncode False . BSS.pack $ p), ("hash", h)]
                    let resp = decode info :: Maybe PhotoResponse
                        photos :: Maybe [Photo]
                        photos = resp >>= (\(PhotoResponse x) -> decode . encode $ x)
                        toAttachment = \(Photo o i) -> "photo" ++ show o ++ "_" ++ show i
                    return $ maybe "" (toAttachment . head) $ photos
