module VK.Base where

import Config
import Control.Lens ((^.))
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Network.Wreq as W
import qualified Web.VKHS as V
import System.Log.Logger
import qualified Data.ByteString.Lazy as BS
import qualified Control.Exception as E
import Network.HTTP.Client
import Control.Concurrent (threadDelay)

type Dispatcher = String -> String -> String -> [(String, String)] -> IO BS.ByteString

data VKData = VKData
    { accessToken :: String
    , expireDate :: Maybe UTCTime
    , accessRights :: [V.AccessRight]
    , dispatcher :: Dispatcher
    , apiVersion :: String
    , lastMessageID :: Integer
    }

type MonadVK m = (MonadConfig m, MonadState VKData m, MonadIO m)

defaultDispatcher :: Dispatcher
defaultDispatcher at ver method args = do
    r <- W.get toUrl `E.catch` handler
    return $ r ^. W.responseBody
    where
        toUrl = foldl (\a b -> a ++ "&" ++ b) ("https://api.vk.com/method/" ++ method ++ "?v=" ++ ver) params
        params = map (\(x, y) -> x ++ "=" ++ y) withat
        withat = ("access_token", at):args
        handler :: HttpException -> IO (Response BS.ByteString)
        handler e = do
            warningM rootLoggerName $ "Dispatcher exception: " ++ show e ++ "! Trying again"
            threadDelay 3000000
            W.get toUrl `E.catch` handler

defaultVKData :: VKData
defaultVKData = VKData "" Nothing [V.Messages] defaultDispatcher "5.40" 0

dispatch :: MonadVK m => String -> [(String, String)] -> m BS.ByteString
dispatch method args = do
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
    liftIO $ d at ver method args

login :: MonadVK m => m ()
login = do
    vklog  <- vkLogin <$> ask
    vkpass <- vkPass <$> ask
    aid    <- appID <$> ask
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
