module VK where

import Config
import Control.Lens
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import qualified Network.Wreq as W
import qualified Web.VKHS as V
import System.Log.Logger
import qualified Data.ByteString.Lazy.Char8 as BS

type Dispatcher = String -> String -> String -> [(String, String)] -> IO (String)

data VKData = VKData 
    { accessToken :: String
    , accessRights :: [V.AccessRight]
    , dispatcher :: Dispatcher 
    , apiVersion :: String
    }

type MonadVK m = (MonadConfig m, MonadState VKData m, MonadIO m)

defaultDispatcher :: Dispatcher
defaultDispatcher at ver method args = do
    r <- W.get toUrl
    return . BS.unpack $ r ^. W.responseBody
    where toUrl = foldl (\a b -> a ++ "&" ++ b) ("https://api.vk.com/method/" ++ method ++ "?v=" ++ ver) params
          params = map (\(x, y) -> x ++ "=" ++ y) withat
          withat = [("access_token", at)] ++ args

defaultVKData :: VKData
defaultVKData = VKData "" [V.Messages] defaultDispatcher "5.40"

runVK :: StateT VKData m a -> m a
runVK m = evalStateT m defaultVKData

dispatch :: MonadVK m => String -> [(String, String)] -> m (String)
dispatch method args = do
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
        Right (at, _, _) -> modify (\x -> x { accessToken = at })
