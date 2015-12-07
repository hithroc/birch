module VK.Messages where

import VK
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.Ether.Implicit

data ID = UserID Int | ChatID Int
    deriving Show
data InMessage = InMessage
    { msgID :: Int
    , uid :: ID
    , message :: String
    }
    deriving Show

instance FromJSON InMessage where
    parseJSON (Object v) = do
        mid <- v .: "id"
        userid <- v .: "user_id"
        chatid <- v .:? "chat_id"
        body <- v .: "body"
        return $ InMessage (read mid) (maybe (UserID $ read userid) (ChatID . read) chatid) body

getMessages :: MonadVK m => m ([InMessage])
getMessages = do
    lid <- lastMessageID <$> get
    r <- dispatch "messages.get" [("last_message_id", show lid), ("count", "20")]
    let resp :: Maybe Value
        resp = decode r
        msgs :: Maybe [Value]
        msgs = resp ^. key "response" . key "items"
    return $ maybe [] id $ join $ map (map $ encode . decode) msgs
