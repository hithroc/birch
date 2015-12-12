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
import VK.Users
import Control.Concurrent
import Control.Exception as E
import System.Exit
import Data.List
import Data.Acid
import CardPictureDB

data Command
    = Version
    | Help
    deriving (Show, Read)

data ProcessedMessage
    = CardRequest String
    | CommandRequest Command
    deriving (Show)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    initLogger
    relaunch
    where
        handlers :: Int -> [Handler ()]
        handlers i = [E.Handler interruptHandler, E.Handler exitHandler, E.Handler $ mainHandler i]
        relaunch = main' `E.catches` handlers 5
        main' = do
            cfg <- loadConfig "config.json"
            let initCfg :: (MonadConfig m, MonadIO m) => m ()
                initCfg = do
                    updateSets
                    locs <- locales <$> get
                    cards <- traverse (readCards . Locale) locs
                    let c = Map.fromList $ zip locs cards
                    evalStateT (runReaderT initVK c) defaultVKData
                initVK :: (MonadVK m, MonadIO m, MonadCardsDB m) => m ()
                initVK = do
                    login
                    myUser <- getUser Nothing
                    case myUser of
                        Nothing -> liftIO $ warningM rootLoggerName "Failed to fetch user's name!"
                        Just myUser' -> modify (\x -> x { logUser = myUser' })
                    loop
            case cfg of
                Nothing -> do
                    errorM rootLoggerName "Failed to load config.json!"
                    exitFailure
                Just cfg' -> void $ evalStateT initCfg cfg'
        mainHandler :: Int -> SomeException -> IO ()
        mainHandler 0 e = do
            emergencyM rootLoggerName $ "It's dead, Jim"
            error "The program is completely crashed"
        mainHandler i e = do
            criticalM rootLoggerName $ "The program crashed with an exception: "
                                     ++ E.displayException e
                                     ++ "! Fix this!"
            infoM rootLoggerName $ "Recovering from crash"
            main' `E.catches` handlers (i-1)
        exitHandler :: ExitCode -> IO ()
        exitHandler (ExitFailure _) = exitFailure
        exitHandler _ = relaunch

        interruptHandler UserInterrupt = return ()
        interruptHandler _ = relaunch

loop :: (MonadVK m, MonadCardsDB m) => m ()
loop = do
    (VKUser _ uname _) <- logUser <$> get
    msgs <- getLongPoll
    let msgs' = map (\x -> (processMessage (uname ++ ", ") x, x)) msgs
    traverse_ (executeMessage) msgs'
    loop


command :: String -> Command
command "version" = Version
command _ = Help

processMessage :: String -> Message -> ProcessedMessage
processMessage prefix msg
    | prefix `isPrefixOf` message msg = CommandRequest . command $ drop (length prefix) (message msg)
    | otherwise = CardRequest $ message msg

prio = [collectible, isSpell, isWeapon, isMinion, isHero, isHeroPower]

executeMessage :: (MonadVK m, MonadCardsDB m) => (ProcessedMessage, Message) -> m ()
executeMessage ((CommandRequest cmd), msg) = do
    sendMessage (Message 0 (uid msg) (show cmd) [])

executeMessage ((CardRequest req), msg) = do
    a <- aliases <$> get
    let parsedCards = parseCards . message $ msg
        aliasedCards = map (\(t, n) -> (t, fromMaybe n $ Map.lookup (map toUpper n) a)) parsedCards
    filtCards <- traverse (processCard prio) aliasedCards
    attachments <- traverse (getCardImage) filtCards
    let retmsg = Message 0 (uid msg) (unlines . map printCard $ filtCards) attachments
    sendMessage retmsg

{-
main :: IO ()
main = do
    case cfg of
        Nothing -> criticalM rootLoggerName "Failed to load config.json!"
        Just conf -> runReaderT main' conf
-}
initLogger :: IO ()
initLogger = do
    let form = simpleLogFormatter "$time [$prio]\t$msg"
    fhand <- fileHandler "./vkbot.log" DEBUG
    shand <- streamHandler stderr WARNING
    let fhand' = setFormatter fhand form
        shand' = setFormatter shand form
    updateGlobalLogger rootLoggerName $ setHandlers [fhand', shand']
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
{-

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
            traverse_ processCardsInMessage msgs
            loop

processCardsInMessage :: (MonadVK m, MonadCardsDB m) => Message -> m ()
processCardsInMessage msg = do
    where
-}

getCardImage :: (MonadVK m, MonadCardsDB m) => Card -> m (String)
getCardImage c = do
    url <- imageURL <$> get
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
