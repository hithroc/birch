module VK.Messages where

import VK.Base
import Data.Aeson
import Data.List
import Control.Monad
import Control.Monad.Ether.Implicit
import Control.Monad.Trans

data ID = UserID Int | ChatID Int
    deriving Show
data Message = Message
    { msgID :: Integer
    , uid :: ID
    , message :: String
    , attachment :: [String]
    }
    deriving Show
data MessageResponse = MessageResponse [Message]
    deriving Show

instance FromJSON Message where
    parseJSON (Object v) = do
        mid <- v .: "id"
        userid <- v .: "user_id"
        chatid <- v .:? "chat_id"
        body <- v .: "body"
        return $ Message mid (maybe (UserID userid) ChatID chatid) body []
    parseJSON _ = mzero

instance FromJSON MessageResponse where
    parseJSON (Object v) = do
        resbody <- v .: "response"
        items <- resbody .: "items"
        return $ MessageResponse items
    parseJSON _ = mzero

getMessages :: MonadVK m => m [Message]
getMessages = do
    lid <- lastMessageID <$> get
    r <- dispatch "messages.get" [("last_message_id", show lid), ("count", "20")]
    let msgs = maybe [] (\(MessageResponse x) -> x) (decode r :: Maybe MessageResponse)
    unless (null msgs) $ modify (\x -> x {lastMessageID = maximum $ map msgID msgs})
    return msgs

sendMessage :: MonadVK m => Message -> m ()
sendMessage msg = do
    let recv = case uid msg of
            UserID i -> ("user_id", show i)
            ChatID i -> ("chat_id", show i)
        args = [("message", message msg)
               , recv]
        withattach = args ++ if not $ null (attachment msg) then
                [("attachment", concat . intersperse "," . attachment $ msg)]
            else
                []
    _ <- dispatch "messages.send" withattach
    return ()
