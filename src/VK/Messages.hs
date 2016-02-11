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
import VK.Types
import Control.Concurrent.STM

getMessages :: MonadVK m => m [Message]
getMessages = do
    tdata <- ask
    let atomdata = liftIO . atomically . readTVar $ tdata
    lid <- lastMessageID <$> atomdata
    r <- dispatch "messages.get" [("last_message_id", show lid), ("count", "20")]
    let msgs = maybe [] (\(MessageResponse x) -> x) (decode r :: Maybe MessageResponse)
    unless (null msgs) $ liftIO . atomically . modifyTVar tdata $ \x -> x {lastMessageID = maximum $ map msgID msgs}
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
        withfwd = withattach ++ if not $ null (forwarded msg) then
                [("forward_messages", concat . intersperse "," . map show . forwarded $ msg)]
            else
                []
    r <- dispatch "messages.send" withfwd
    liftIO . infoM rootLoggerName $ "Message sent! Response was: " ++ show r
    return ()

intToID :: Integer -> Map.Map String LongPollValue -> ID
intToID i m
    | i > 2000000000 = ChatID (fromMaybe 0 (textdata <$> Map.lookup "from" m >>= readMaybe)) $ i - 2000000000
    | otherwise = UserID i

longToMsg :: [LongPollValue] -> Maybe Message
longToMsg [] = Nothing
longToMsg v = 
    if intdata (head v) == 4
    then Just $ Message (intdata (v !! 1)) (intToID (intdata (v !! 3)) (objdata (v !! 7))) (textdata (v !! 6)) [] []
    else Nothing

getLongPoll :: MonadVK m => m [Message]
getLongPoll = do
    tdata <- ask
    let atomdata = liftIO . atomically . readTVar $ tdata
    lps <- longPollServer <$> atomdata
    case lps of
        Nothing -> do
            r <- dispatch "messages.getLongPollServer" []
            liftIO $ infoM rootLoggerName "Connected to a Long Poll server"
            liftIO . atomically . modifyTVar tdata $ (\x -> x {longPollServer = decode r})
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
            case decode (r ^. W.responseBody) of
                Nothing -> do
                    liftIO $ infoM rootLoggerName "LongPoll server key expired (most likely). Retrying"
                    liftIO . atomically . modifyTVar tdata $ (\x -> x {longPollServer = Nothing})
                    getLongPoll
                Just (LongPollResponse msgs ts) -> do
                    liftIO . atomically . modifyTVar tdata $ (\x -> x {longPollServer = Just (s { lpsts = ts })})
                    let msgs' = mapMaybe longToMsg msgs
                    unless (null msgs') $ liftIO . atomically . modifyTVar tdata $ (\x -> x {lastMessageID = maximum $ map msgID msgs'})
                    return msgs'
