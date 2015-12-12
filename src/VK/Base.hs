module VK.Base where

import Config
import Control.Lens ((^.), (.~),(&))
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import Data.Aeson
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Network.Wreq as W
import qualified Web.VKHS as V
import System.Log.Logger
import qualified Data.ByteString.Lazy as BS
import qualified Control.Exception as E
import Network.HTTP.Client
import qualified Data.Text as T

type Dispatcher = String -> String -> String -> [(String, String)] -> IO BS.ByteString
data LongPollServer = LongPollServer { lpskey :: String, lpsurl :: String, lpsts :: Integer }

instance FromJSON LongPollServer where
    parseJSON (Object v) = do
        resp <- v .: "response"
        LongPollServer <$> resp .: "key"
                       <*> resp .: "server"
                       <*> resp .: "ts"
    parseJSON _ = mzero

data VKData = VKData
    { accessToken :: String
    , expireDate :: Maybe UTCTime
    , accessRights :: [V.AccessRight]
    , dispatcher :: Dispatcher
    , apiVersion :: String
    , lastMessageID :: Integer
    , longPollServer :: Maybe LongPollServer
    }


type MonadVK m = (MonadConfig m, MonadState VKData m, MonadIO m)

defaultDispatcher :: Dispatcher
defaultDispatcher at ver meth args = do
    r <- W.getWith opts url `E.catch` handler
    return $ r ^. W.responseBody
    where
        url = "https://api.vk.com/method/" ++ meth
        opts = foldl (&) W.defaults $ map (\(x, y) -> W.param (T.pack x) .~ [T.pack y]) args'
        args' = [("v",ver), ("access_token", at)] ++ args
        handler :: HttpException -> IO (Response BS.ByteString)
        handler e = do
            warningM rootLoggerName $ "Dispatcher exception: " ++ show e ++ "! Trying again"
            W.getWith opts url `E.catch` handler

defaultVKData :: VKData
defaultVKData = VKData "" Nothing [V.Messages, V.Photos] defaultDispatcher "5.40" 0 Nothing

dispatch :: MonadVK m => String -> [(String, String)] -> m BS.ByteString
dispatch meth args = do
    expdate <- expireDate <$> get
    curtime <- liftIO $ getCurrentTime
    case expdate of
        Just t -> when (diffUTCTime t curtime < 120) $ do
                liftIO . infoM rootLoggerName $ "Access token is about to expire, relogging..."
                login
        Nothing -> liftIO . warningM rootLoggerName $ "No token expiration time set!"
    d <- dispatcher <$> get
    at <- accessToken <$> get
    ver <- apiVersion <$> get
    liftIO $ d at ver meth args

login :: MonadVK m => m ()
login = do
    vklog  <- vkLogin <$> get
    vkpass <- vkPass <$> get
    aid    <- appID <$> get
    rs <- accessRights <$> get
    let e = V.env aid vklog vkpass rs
    at' <- liftIO $ V.login e
    case at' of
        Left s -> liftIO . errorM rootLoggerName $ "VK Login error: " ++ s
        Right (at, uid, expstr) -> do
            liftIO . infoM rootLoggerName $ "Login as " ++ uid ++ " successfull"
            curtime <- liftIO $ getCurrentTime
            let expdate = addUTCTime (fromIntegral . read $ expstr) curtime
            tz <- liftIO $ getCurrentTimeZone
            liftIO . infoM rootLoggerName $ "Token expires at " ++ show (utcToLocalTime tz expdate) ++ " " ++ show tz
            modify (\x -> x { accessToken = at, expireDate = Just expdate })
