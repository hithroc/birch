module VK.Photos where

import VK.Base
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as BS
import VK.Types

-- TODO: Rewrite this
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
                    info <- dispatch "photos.saveMessagesPhoto" [("server", show s), ("photo", p), ("hash", h)]
                    let resp = decode info :: Maybe PhotoResponse
                        photos :: Maybe [Photo]
                        photos = resp >>= (\(PhotoResponse x) -> decode . encode $ x)
                        toAttachment = \(Photo o i) -> "photo" ++ show o ++ "_" ++ show i
                    return $ maybe "" (toAttachment . head) $ photos
