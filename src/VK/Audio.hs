module VK.Audio where

import VK.Types
import VK.Base
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import System.Process
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as BS

data AudioServ = AudioServ String
data AudioUploadResponse = AudioUploadResponse Integer String String
data AudioResponse = AudioResponse Audio
data Audio = Audio Integer Integer
    deriving Show

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

instance FromJSON Audio where
    parseJSON (Object v) = do
        res <- v .: "response"
        o <- res .: "owner_id"
        i <- res .: "id"
        return $ Audio o i
    parseJSON _ = mzero

uploadAudio :: MonadVK m => String -> String -> BS.ByteString -> m (String)
uploadAudio artist title raw' = do
    let filename = artist ++ title ++ ".mp3"
    liftIO $ BS.writeFile filename raw'
    _ <- liftIO $ readProcess "sox" [filename, "_" ++ filename, "pad", "0", "5"] ""
    raw <- liftIO $ BS.readFile $ "_" ++ filename
    serv <- dispatch "audio.getUploadServer" []
    case decode serv of
        Nothing -> return ""
        Just (AudioServ servstr) -> do
            r <- liftIO $ W.post servstr [W.partLBS "file" raw & W.partFileName .~ Just "soundfile.mp3"]
            case decode (r ^. W.responseBody) of
                Nothing -> return ""
                Just (AudioUploadResponse s a h) -> do
                    info <- dispatch "audio.save" [("server", show s), ("audio", a), ("hash", h), ("artist", artist), ("title", title)]
                    let resp = decode info :: Maybe Audio
                        toAttachment = \(Audio o i) -> "audio" ++ show o ++ "_" ++ show i
                    return $ maybe "" (toAttachment) $ resp
