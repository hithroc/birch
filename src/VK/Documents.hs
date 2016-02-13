module VK.Documents where

import VK.Types
import VK.Base
import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import qualified Network.Wreq as W
import qualified Data.ByteString.Lazy as BS


-- TODO: Rewrite this
uploadDocument :: MonadVK m => String -> BS.ByteString -> m (String)
uploadDocument filename raw = do
    serv <- dispatch "docs.getUploadServer" []
    case decode serv of
        Nothing -> return ""
        Just (PhotoServ servstr) -> do
            r <- liftIO $ W.post servstr [W.partLBS "file" raw & W.partFileName .~ Just filename]
            case decode (r ^. W.responseBody) of
                Nothing -> return ""
                Just (DocUploadResponse f) -> do
                    info <- dispatch "docs.save" [("file", f), ("title", filename)]
                    liftIO $ print info
                    let resp = decode info :: Maybe DocResponse
                        toAttachment = \(DocResponse o i) -> "doc" ++ show o ++ "_" ++ show i
                    return $ maybe "" toAttachment $ resp
