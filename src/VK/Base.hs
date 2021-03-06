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
import Network.HTTP.Client.TLS
import qualified Data.Text as T
import VK.Types
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import System.Posix.Daemonize (fatalError)

defaultDispatcher :: Dispatcher
defaultDispatcher at ver meth args = do
    let opts' = opts & W.manager .~ Left (tlsManagerSettings { managerResponseTimeout = Just 10000 } )
    r <- W.getWith opts' url `E.catch` handler
    case decode (r ^. W.responseBody) of
        Nothing -> do
            return . Right $ r ^. W.responseBody
        Just (ErrorResponse code msg uri) -> case code of
            -- Flood Control
            9 -> do
                infoM rootLoggerName $ "Recieved flood control."
                threadDelay 30000000
                return (Right "")
            17 -> do
                criticalM rootLoggerName $ "Redirect URI required (" ++ show code ++ ")"
                criticalM rootLoggerName uri
                return (Right "")
            100 -> do
                warningM rootLoggerName $ "Empty request: " ++ meth ++ ":" ++ show args
                return (Right "")
            _ -> return (Left (code, msg))
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
defaultVKData = VKData "" Nothing [V.Messages, V.Photos, V.Audio, V.Docs, V.Friends] defaultDispatcher "5.40" 0 Nothing (VKUser (UserID 0) "" "") (Map.empty)

dispatch :: MonadVK m => String -> [(String, String)] -> m BS.ByteString
dispatch meth args = do
    tdata <- ask
    let atomized = liftIO . atomically . readTVar $ tdata
    expdate <- expireDate <$> atomized
    curtime <- liftIO $ getCurrentTime
    case expdate of
        Just t -> when (diffUTCTime t curtime < 120) $ do
                liftIO . infoM rootLoggerName $ "Access token is about to expire, relogging..."
                login
        Nothing -> liftIO . warningM rootLoggerName $ "No token expiration time set!"
    d <- dispatcher <$> atomized
    at <- accessToken <$> atomized
    ver <- apiVersion <$> atomized
    ans <- liftIO $ d at ver meth args
    case ans of
        Right x -> return x
        Left (5, _) -> do
            liftIO . infoM rootLoggerName $ "Access token expired!"
            login
            dispatch meth args
        Left (code, msg) -> do
                liftIO $ infoM rootLoggerName $ "VK gave a response with error: " ++ msg ++ "(" ++ show code ++ ")"
                liftIO $ threadDelay 30000000
                dispatch meth args

login :: MonadVK m => m ()
login = do
    (tcfg :: TVar Config) <- ask
    (tdata :: TVar VKData) <- ask
    let atomized = liftIO . atomically . readTVar $ tcfg
    let atomdata = liftIO . atomically . readTVar $ tdata
    vklog  <- vkLogin <$> atomized
    vkpass <- vkPass <$> atomized
    aid    <- appID <$> atomized
    rs <- accessRights <$> atomdata
    let e = V.env aid vklog vkpass rs
    at' <- liftIO $ V.login e
    case at' of
        Left s -> do
            let logmsg = "VK login error: " ++ s
            liftIO $ errorM rootLoggerName logmsg
            fatalError logmsg
        Right (at, suid, expstr) -> do
            liftIO . infoM rootLoggerName $ "Login as " ++ suid ++ " successfull"
            curtime <- liftIO $ getCurrentTime
            let expdate = addUTCTime (fromIntegral (read expstr :: Integer)) curtime
            tz <- liftIO $ getCurrentTimeZone
            liftIO . infoM rootLoggerName $ "Token expires at " ++ show (utcToLocalTime tz expdate) ++ " " ++ show tz
            liftIO . atomically . modifyTVar tdata $ \x -> x { accessToken = at, expireDate = Just expdate }
