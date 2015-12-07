module VK.Messages where

import VK
import Data.Aeson
import Data.Maybe
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import System.Log.Logger
import qualified Data.HashMap.Strict as Map

data ID = UserID Int | ChatID Int
    deriving Show
data InMessage = InMessage
    { msgID :: Integer
    , uid :: ID
    , message :: String
    }
    deriving Show
data MessageResponse = MessageResponse [InMessage]
    deriving Show

instance FromJSON InMessage where
    parseJSON (Object v) = do
        mid <- v .: "id"
        userid <- v .: "user_id"
        chatid <- v .:? "chat_id"
        body <- v .: "body"
        return $ InMessage (mid) (maybe (UserID userid) ChatID chatid) body

instance FromJSON MessageResponse where
    parseJSON (Object v) = do
        resbody <- v .: "response"
        items <- resbody .: "items"
        return $ MessageResponse items

getMessages :: MonadVK m => m ([InMessage])
getMessages = do
    lid <- lastMessageID <$> get
    r <- dispatch "messages.get" [("last_message_id", show lid), ("count", "20")]
    liftIO $ print r
    let msgs = maybe [] (\(MessageResponse x) -> x) (decode r :: Maybe MessageResponse)
    when (not $ null msgs) $ modify (\x -> x {lastMessageID = maximum $ map (msgID) msgs})
    return msgs
