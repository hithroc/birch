module VK.Messages where

import VK.Base
import Data.Aeson
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import System.Log.Logger
import qualified Network.Wreq as W
import qualified Data.Text as T

data ID = UserID Integer | ChatID Integer
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

data LongPollValue 
    = IntData  { intdata :: Integer }
    | TextData { textdata :: String }
    | ObjData  { objdata :: Value }

data LongPollResponse = LongPollResponse [[LongPollValue]] Integer

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

instance FromJSON LongPollValue where
    parseJSON (Number n) = return $ IntData (truncate n)
    parseJSON (String t) = return $ TextData (T.unpack t) 
    parseJSON (Object v) = return $ ObjData (Object v)
    parseJSON _ = mzero

instance FromJSON LongPollResponse where
    parseJSON (Object v) = LongPollResponse
                        <$> v .: "updates"
                        <*> v .: "ts"
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


intToID :: Integer -> ID
intToID i
    | i > 2000000000 = ChatID $ i - 2000000000
    | otherwise = UserID i

longToMsg :: [LongPollValue] -> Maybe Message
longToMsg [] = Nothing
longToMsg v = 
    if intdata (head v) == 4
    then Just $ Message (intdata (v !! 1)) (intToID $ intdata (v !! 3)) (textdata (v !! 6)) []
    else Nothing

getLongPoll :: MonadVK m => m [Message]
getLongPoll = do
    lps <- longPollServer <$> get
    case lps of
        Nothing -> do
            r <- dispatch "messages.getLongPollServer" []
            liftIO $ print $ r
            modify (\x -> x {longPollServer = decode r})
            getLongPoll
        Just s -> do
            let url = "http://" ++ lpsurl s ++ "?act=a_check&key=" ++ lpskey s ++ "&ts=" ++ show (lpsts s) ++ "&wait=25&mode=2"
            r <- liftIO $ W.get url
            case decode (r ^. W.responseBody) of
                Nothing -> do
                    liftIO $ infoM rootLoggerName "LongPoll server key expired (most likely). Retrying"
                    modify (\x -> x {longPollServer = Nothing})
                    getLongPoll
                Just (LongPollResponse msgs ts) -> do
                    modify (\x -> x {longPollServer = Just (s { lpsts = ts })})
                    return $ mapMaybe longToMsg msgs

