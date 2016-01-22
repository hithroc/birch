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
import VK.Types
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)

defaultDispatcher :: Dispatcher
defaultDispatcher at ver meth args = do
    infoM rootLoggerName $ "Dispatching " ++ meth ++ " with " ++ show args
    r <- W.getWith opts url `E.catch` handler
    case decode (r ^. W.responseBody) of
        Nothing -> do
            infoM rootLoggerName $ "Dispatched!"
            return $ r ^. W.responseBody
        Just (ErrorResponse code msg) -> case code of
            -- Flood Control
            9 -> do
                infoM rootLoggerName $ "Recieved flood control."
                threadDelay 30000000
                defaultDispatcher at ver meth args
            _ -> do
                infoM rootLoggerName $ "VK gave a response with error: " ++ msg ++ "(" ++ show code ++ ")"
                threadDelay 30000000
                defaultDispatcher at ver meth args
    where
        url = "https://api.vk.com/method/" ++ meth
        opts = foldl (&) W.defaults $ map (\(x, y) -> W.param (T.pack x) .~ [T.pack y]) args'
        args' = [("v",ver), ("access_token", at)] ++ args
        handler :: HttpException -> IO (Response BS.ByteString)
        handler (FailedConnectionException _ _) = W.getWith opts url `E.catch` handler
        handler e = do
            warningM rootLoggerName $ "Dispatcher exception: " ++ show e ++ "! Trying again"
            W.getWith opts url `E.catch` handler

defaultVKData :: VKData
defaultVKData = VKData "" Nothing [V.Messages, V.Photos, V.Audio] defaultDispatcher "5.40" 0 Nothing (VKUser (UserID 0) "" "") (Map.empty)

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
