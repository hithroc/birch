module Main where

import Config
import Card
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
import VK.Messages
import VK.Photos
import Control.Concurrent

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

main' :: (MonadConfig m, MonadIO m) => m ()
main' = do
    updateSets
    locs <- locales <$> ask
    cards <- traverse readCards locs
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
            msgs <- getMessages
            traverse_ answer msgs
            loop
        answer :: (MonadVK m, MonadCardsDB m) => Message -> m ()
        answer msg = do
            filtCards <- traverse (prepareCard)
                       . getCards
                       $ message msg
            let prepcards = catMaybes $ filtCards
            attachments <- traverse (getAttachment) prepcards
            let retmsg = Message 0 (uid msg) (unlines . map printCard $ prepcards) attachments
            sendMessage retmsg
        getAttachment :: (MonadVK m, MonadCardsDB m) => Card -> m (String)
        getAttachment c = do
            url <- imageURL <$> ask
            r <- liftIO . W.get $ url ++ "/enus/original/" ++ cardID c ++ ".png"
            uploadPhoto (r ^. W.responseBody)

        prio = [collectible, isSpell, isWeapon, isMinion, isHero, isHeroPower]

        prepareCard :: (MonadCardsDB m, MonadIO m) => (S.Set CardTag, String) -> m (Maybe Card)
        prepareCard (tags, n) = do
            loccards <- searchBy name n
            let cards = concatMap snd . Map.toList $ loccards
            if null cards then do
                liftIO . infoM rootLoggerName $ "No card named \"" ++ n ++ "\" found!"
                return Nothing
            else do
                let resultcard = priority cards prio
                Just <$> foldl (\a b -> a >>= dealWithTag b) (return resultcard) tags
            
        dealWithTag :: MonadCardsDB m => CardTag -> Card -> m Card
        dealWithTag (Locale l) c = do
            cards <- exactSearchBy cardID $ cardID c
            case Map.lookup l cards >>= listToMaybe of
                Nothing -> return c
                Just c' -> return c'
        dealWithTag _ c = return c
