module VK.Messages where

import VK.Base
import Data.Aeson
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
import Control.Lens
import Control.Monad
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import System.Log.Logger
import Network.HTTP.Client (HttpException(..))
import qualified Network.Wreq as W
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

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
    | ObjData  { objdata :: Map.Map String LongPollValue }
    deriving (Ord, Eq)

data LongPollResponse = LongPollResponse [[LongPollValue]] Integer

instance FromJSON Message where
    parseJSON (Object v) = do
        mid <- v .: "id"
        userid <- v .: "user_id"
        chatid <- v .:? "chat_id"
        body <- v .: "body"
        return $ Message mid (maybe (UserID userid) (ChatID userid) chatid) body []
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
            ChatID _ i -> ("chat_id", show i)
        args = [("message", message msg)
               , recv]
        withattach = args ++ if not $ null (attachment msg) then
                [("attachment", concat . intersperse "," . attachment $ msg)]
            else
                []
    _ <- dispatch "messages.send" withattach
    return ()


intToID :: Integer -> Map.Map String LongPollValue -> ID
intToID i m
    | i > 2000000000 = ChatID (fromMaybe 0 (textdata <$> Map.lookup "from" m >>= readMaybe)) $ i - 2000000000
    | otherwise = UserID i

longToMsg :: [LongPollValue] -> Maybe Message
longToMsg [] = Nothing
longToMsg v = 
    if intdata (head v) == 4
    then Just $ Message (intdata (v !! 1)) (intToID (intdata (v !! 3)) (objdata (v !! 7))) (textdata (v !! 6)) []
    else Nothing

getLongPoll :: MonadVK m => m [Message]
getLongPoll = do
    lps <- longPollServer <$> get
    case lps of
        Nothing -> do
            r <- dispatch "messages.getLongPollServer" []
            liftIO $ infoM rootLoggerName "Connected to a Long Poll server"
            modify (\x -> x {longPollServer = decode r})
            getLongPoll
        Just s -> do
            let url = "https://" ++ lpsurl s ++ "?act=a_check&key=" ++ lpskey s ++ "&ts=" ++ show (lpsts s) ++ "&wait=25&mode=2"
            let catchhandler (ResponseTimeout) = autocatch
                catchhandler e = do
                    warningM rootLoggerName $ "Fetching LongPoll data exception: " ++ show e
                    autocatch
                autocatch :: IO (W.Response BS.ByteString)
                autocatch = W.get url `E.catch` catchhandler
            r <- liftIO $ autocatch
            liftIO$print$r^.W.responseBody
            case decode (r ^. W.responseBody) of
                Nothing -> do
                    liftIO $ infoM rootLoggerName "LongPoll server key expired (most likely). Retrying"
                    modify (\x -> x {longPollServer = Nothing})
                    getLongPoll
                Just (LongPollResponse msgs ts) -> do
                    modify (\x -> x {longPollServer = Just (s { lpsts = ts })})
                    let msgs' = mapMaybe longToMsg msgs
                    unless (null msgs') $ modify (\x -> x {lastMessageID = maximum $ map msgID msgs'})
                    return msgs'

