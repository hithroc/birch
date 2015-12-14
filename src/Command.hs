module Command where

import Card
import Version
import VK
import Config
import Data.List
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import System.Log.Logger
import Data.Char
import Data.Maybe
import Data.Acid
import CardPictureDB
import Control.Lens
import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Network.Wreq as W

data Command
    = Version
    | Reload
    | Quote
    | CardRequest Message

parseCommand :: MonadVK m => Message -> m Command
parseCommand msg@(Message {message = msgtext}) = do
    (VKUser _ uname _) <- logUser <$> get
    let prefix = uname ++ ", "
    return $ case fmap words (prefix `stripPrefix` msgtext) of
        Just ("version":_) -> Version
        Just ("reload":_) -> Reload
        Just ("quote":_) -> Quote
        Nothing -> CardRequest msg
        Just _ -> CardRequest msg

checkPermission :: MonadVK m => ID -> m (Bool)
checkPermission i = do
    adm <- admins <$> get
    return $ (userID i) `elem` adm

withPermission :: MonadVK m => ID -> m () -> m ()
withPermission vid f = do
    perm <- checkPermission vid
    if perm 
    then f
    else sendMessage $ Message 0 vid ("You have no permission to execute that command") []

prio = [collectible, isSpell, isWeapon, isMinion, isHero, isHeroPower]

execute :: (MonadVK m, MonadCardsDB m) => ID -> Command -> m ()
execute vid Version = sendMessage (Message 0 vid ("Current version: " ++ version) [])
execute vid Reload = do
    let f = do
            cfg <- liftIO $ loadConfig "config.json"
            case cfg of
                Nothing -> sendMessage $ Message 0 vid "Failed to reload config" []
                Just cfg' -> do
                    modify (const cfg')
                    sendMessage $ Message 0 vid "Config reloaded" []
    withPermission vid f

execute vid (CardRequest msg) = do
    a <- aliases <$> get
    let parsedCards = parseCards . message $ msg
        aliasedCards = map (\(t, n) -> (t, fromMaybe n $ Map.lookup (map toUpper n) a)) parsedCards
    filtCards <- traverse (processCard prio) aliasedCards
    attachments <- traverse (getCardImage) filtCards
    let retmsg = Message 0 vid (unlines . map printCard $ filtCards) attachments
    sendMessage retmsg

execute vid _ = sendMessage $ Message 0 vid "Command is not implemented" []

getCardImage :: (MonadVK m, MonadCardsDB m) => Card -> m (String)
getCardImage c = do
    url <- imageURL <$> get
    let (Locale loc) = locale c
    let imgurl = url ++ map toLower loc ++ "/original/" ++ cardID c ++ ".png"
    acid <- liftIO $ openLocalState (CardPics Map.empty)
    mbPic <- liftIO $ query acid (GetPic imgurl)
    pid <- case if isNotFound c then Just "" else mbPic of
        Nothing -> do
            r <- liftIO $ ((Right <$> W.get imgurl) `E.catch` \(e :: E.SomeException) -> return (Left e))
            case r of
                Right r' -> do
                    pid <- uploadPhoto (r' ^. W.responseBody)
                    unless (null pid) . liftIO $ update acid (InsertPic imgurl pid)
                    return pid
                Left e -> do
                    liftIO . infoM rootLoggerName $ "No image for " ++ name c ++ " found"
                    return ""
        Just pid -> do
            return pid
    liftIO $ closeAcidState acid
    return pid
