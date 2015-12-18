module VK.Audio where

import VK.Types
import VK.Base
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as BS

data AudioServ = AudioServ String
data AudioUploadResponse = AudioUploadResponse Integer String String
data AudioResponse = AudioResponse [Audio]
data Audio = Audio String String

instance FromJSON AudioServ where
    parseJSON (Object v) = do
        res <- v .: "response"
        url <- res .: "upload_url"
        return $ AudioServ url
    parseJSON _ = mzero

instance FromJSON AudioUploadResponse where
    parseJSON (Object v) = AudioUploadResponse 
                        <$> v .: "server"
                        <*> v .: "audio"
                        <*> v .: "hash"
    parseJSON _ = mzero

instance FromJSON AudioResponse where
    parseJSON (Object v) = do
        res <- v .: "response"
        return $ AudioResponse res
    parseJSON _ = mzero

instance FromJSON Audio where
    parseJSON (Object v) = Audio
                        <$> v .: "owner_id"
                        <*> v .: "id"
    parseJSON _ = mzero


uploadAudio :: MonadVK m => String -> String -> BS.ByteString -> m (String)
uploadAudio artist title raw = do
    serv <- dispatch "audio.getUploadServer" []
    case decode serv of
        Nothing -> return ""
        Just (AudioServ servstr) -> do
            r <- liftIO $ W.post servstr [W.partLBS "file" raw]
            case decode (r ^. W.responseBody) of
                Nothing -> return ""
                Just (AudioUploadResponse s a h) -> do
                    info <- dispatch "audio.save" [("server", show s), ("audio", a), ("hash", h), ("artist", artist), ("title", title)]
                    let resp = decode info :: Maybe AudioResponse
                        audios :: Maybe [Audio]
                        audios = (\(AudioResponse xs) -> xs) <$> resp
                        toAttachment = \(Audio o i) -> "audio" ++ show o ++ "_" ++ show i
                    return $ maybe "" (toAttachment . head) $ audios
