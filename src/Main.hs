module Main where

import Config
import Card
import Data.Char
import Data.Maybe
import Data.Foldable
import qualified Data.Set as S
import Control.Monad.Trans
import Control.Monad.Ether.Implicit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple hiding (priority)
import System.IO
import qualified Data.Map as Map
import qualified Network.Wreq as W
import Control.Lens
import VK
import Control.Concurrent
import Control.Exception as E

import Data.Acid
import CardPictureDB
import Debug.Trace

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initLogger
    cfg <- loadConfig "config.json"
    case cfg of
        Nothing -> criticalM rootLoggerName "Failed to load config.json!"
        Just conf -> runReaderT main' conf

initLogger :: IO ()
initLogger = do
    let form = simpleLogFormatter "$time [$prio]\t$msg"
    fhand <- fileHandler "./vkbot.log" DEBUG
    shand <- streamHandler stderr WARNING
    let fhand' = setFormatter fhand form
        shand' = setFormatter shand form
    updateGlobalLogger rootLoggerName $ setHandlers [fhand', shand']
    updateGlobalLogger rootLoggerName $ setLevel DEBUG

prio = [collectible, isSpell, isWeapon, isMinion, isHero, isHeroPower]

main' :: (MonadConfig m, MonadIO m) => m ()
main' = do
    updateSets
    locs <- locales <$> ask
    cards <- traverse (readCards . Locale) locs
    let c = Map.fromList $ zip locs cards
    evalStateT (runReaderT initVK c) defaultVKData
    where
        initVK :: (MonadVK m, MonadCardsDB m) => m ()
        initVK = do
            login
            liftIO . infoM rootLoggerName $ "Executing main loop"
            loop
        loop :: (MonadVK m, MonadCardsDB m) => m ()
        loop = do
            liftIO $ threadDelay 3000000
            msgs <- getLongPoll
            traverse_ processCardsInMessage msgs
            loop

processCardsInMessage :: (MonadVK m, MonadCardsDB m) => Message -> m ()
processCardsInMessage msg = do
    a <- aliases <$> ask
    let parsedCards = parseCards . message $ msg
        aliasedCards = map (\(t, n) -> (t, fromMaybe n $ Map.lookup (map toUpper n) a)) parsedCards
    filtCards <- traverse (processCard prio) aliasedCards
    attachments <- traverse (getCardImage) filtCards
    let retmsg = Message 0 (uid msg) (unlines . map printCard $ filtCards) attachments
    sendMessage retmsg
    where

getCardImage :: (MonadVK m, MonadCardsDB m) => Card -> m (String)
getCardImage c = do
    url <- imageURL <$> ask
    let (Locale loc) = locale c
    let imgurl = url ++ map toLower loc ++ "/original/" ++ cardID c ++ ".png"
    acid <- liftIO $ openLocalState (CardPics Map.empty)
    mbPic <- liftIO $ query acid (GetPic imgurl)
    pid <- case if isNotFound c then Just "" else mbPic of
        Nothing -> do
            r <- liftIO . W.get $ imgurl
            pid <- uploadPhoto (r ^. W.responseBody)
            liftIO $ update acid (InsertPic imgurl pid)
            return pid
        Just pid -> do
            return pid
    liftIO $ closeAcidState acid
    return pid
