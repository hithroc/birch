module Command where

import Card
import Version
import VK
import Config
import Data.List
import Data.Time.Clock
import Control.Monad.Ether.Implicit
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import System.Log.Logger
import Data.Aeson
import Data.Char
import Data.Maybe
import Data.Acid
import CardPictureDB
import AudioDB
import Control.Lens
import System.Random
import System.Time.Utils (renderSecs)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Network.Wreq as W
import qualified Data.Set as S
import UserManagment

data Command
    = Version
    | Reload
    | Quote
    | Thogar
    | Tectus
    | Status
    | Update
    | CardCraft Rarity
    | CardRequest Message


parseCommand :: MonadVK m => Message -> m Command
parseCommand msg@(Message {message = msgtext}) = do
    (VKUser _ uname _) <- logUser <$> get
    let prefix = uname ++ ", "
    return $ case fmap words (prefix `stripPrefix` msgtext) of
        Just ("version":_) -> Version
        Just ("reload":_) -> Reload
        Just ("quote":_) -> Quote
        Just ("update":_) -> Update
        Just ("thogar":_) -> Thogar
        Just ("tectus":_) -> Tectus
        Just ("status":_) -> Status
        Just (["which","legendary","to","craft"]) -> CardCraft LEGENDARY
        Just (["which","epic","to","craft"]) -> CardCraft EPIC
        Just (["which","rare","to","craft"]) -> CardCraft RARE
        Just (["which","common","to","craft"]) -> CardCraft COMMON
        Nothing -> CardRequest msg
        Just _ -> CardRequest msg

checkPermission :: MonadVK m => Permission -> ID -> m (Maybe String)
checkPermission perm i = do
    (UserPermissions perms) <- userperm <$> get
    let perm' = Map.findWithDefault User (userID i) perms
    let msg = Map.findWithDefault "You don't have a permisison to execute that command" perm' permMsgs
    return (if perm >= perm' then Nothing else Just msg)

withPermission :: MonadVK m => Permission -> ID -> m () -> m ()
withPermission perm vid f = do
    msg <- checkPermission perm vid
    case msg of
        Nothing -> f
        Just msg' -> sendMessage $ Message 0 vid msg' [] []

prio = [collectible, isSpell, isWeapon, isMinion, isHero, isHeroPower]

executeIO :: TVar VKData -> TVar Config -> Cards -> ID -> Command -> IO ()
executeIO tvd tcfg cs id command =  do
    vkdata <- atomically $ readTVar tvd
    cfg <- atomically $ readTVar tcfg
    ((_,vkdata'),cfg') <- runStateT (runStateT (runReaderT (execute id command) cs) vkdata) cfg
    atomically $ writeTVar tcfg cfg'
    atomically $ writeTVar tvd vkdata'

execute :: (MonadVK m, MonadCardsDB m) => ID -> Command -> m ()
execute vid Version = withPermission Everyone vid $ sendMessage (Message 0 vid ("Current version: " ++ version) [] [])
execute vid Reload = withPermission Admin vid $ do
    cfg <- liftIO $ loadConfig "config.json"
    case cfg of
        Nothing -> sendMessage $ Message 0 vid "Failed to reload config" [] []
        Just cfg' -> do
            modify (const cfg')
            sendMessage $ Message 0 vid "Config reloaded" [] []

execute vid (CardRequest msg) = do
    a <- aliases <$> get
    let parsedCards = parseCards . message $ msg
        aliasedCards = map (\(t, n) -> (t, fromMaybe n $ Map.lookup (map toUpper n) a)) parsedCards
    filtCards <- traverse (processCard prio) aliasedCards
    unless (null filtCards) $ do
        pattachments <- traverse (\(tags, x) -> (uncurry $ getCardImage (S.member Golden tags)) x) filtCards
        let audioget (tags, (loc, card)) = do
                let action x = case x of
                        Snd st -> getCardSound loc st card
                        _ -> return ""
                traverse (action) $ S.toList tags
        aattachments <- concat <$> traverse (audioget) filtCards
        let retmsg = Message 0 vid (unlines . map ((uncurry printCard) . snd) $ filtCards) (pattachments ++ aattachments) []
        sendMessage retmsg

execute vid Quote = withPermission User vid $ do
    cooldowns <- quoteCooldown <$> get
    curtime <- liftIO $ getCurrentTime
    cd <- quoteCD <$> get
    let time = fromMaybe (addUTCTime (negate cd) curtime) (Map.lookup vid cooldowns)
        diff = diffUTCTime curtime time
    if diff >= cd then do
        lid <- lastMessageID <$> get
        banned <- bannedForQuote <$> get
        r <- liftIO $ randomRIO (1,lid)
        res <- dispatch "messages.getById" [("message_ids", show r)]
        let msgs = maybe [] (\(MessageResponse x) -> x) (decode res :: Maybe MessageResponse)
        case msgs of
            (x:_) -> do
                if userID (uid x) `elem` banned then do
                    execute vid Quote
                else do
                    modify (\d -> d { quoteCooldown = Map.insert vid curtime cooldowns })
                    sendMessage $ Message 0 vid "Here is a quote for you:" [] [r]
            _ -> execute vid Quote
    else sendMessage $ Message 0 vid ("You have to wait another " ++ renderSecs (truncate $ cd - diff) ++ " before you can fetch another quote") [] []

execute vid Update = withPermission Admin vid $ do
    downloadSet
    sendMessage $ Message 0 vid "Database updated!" [] []

execute vid (CardCraft r) = withPermission User vid $ do
    (cards' :: Cards) <- ask
    let cards = filter (\c -> rarity c == r && collectible c) $ cards'
    if length cards == 0 then
        sendMessage $ Message 0 vid "Something wrong happened" [] []
    else do
        num <- liftIO $ randomRIO (0, length cards - 1)
        execute vid (CardRequest $ Message 0 vid ("[[" ++ (unlocalize (Locale "enUS") $ name (cards !! num)) ++ "]]") [] [])

execute vid Tectus = withPermission Honored vid $ do
    let quoteTectus 0 = sendMessage $ Message 0 vid "Even the mountain... falls..." [] []
        quoteTectus i = do
            r <- liftIO $ randomRIO (1, 7)
            sendMessage $ Message 0 vid ((concat $ replicate r "RI-") ++ "RISE MOUNTAINS") [] []
            liftIO $ threadDelay 2000000
            quoteTectus (i-1)
    sendMessage $ Message 0 vid "What is this?!..." [] []
    liftIO $ threadDelay 2000000
    sendMessage $ Message 0 vid "RISE MOUNTAINS" [] []
    liftIO $ threadDelay 2000000
    sendMessage $ Message 0 vid "RISE MOUNTAINS" [] []
    liftIO $ threadDelay 2000000
    quoteTectus 4

execute vid Thogar = withPermission Honored vid $ do
    let quotes = ["Redball incoming!"
                 ,"Send'er on down the line!"
                 ,"Faster! Bat the stack off her!"
                 ,"Track one."
                 ,"Live iron on track one!"
                 ,"Track two!"
                 ,"Incoming on two."
                 ,"Track three."
                 ,"Track three inbound."
                 ,"Track four."
                 ,"Inbound on four."
                 ,"Express, coming through."
                 ,"Ah - reinforcements."
                 ,"Reinforcements, right on time."
                 ,"Troop train - inbound!"
                 ,"Here they come - hit the grit, boys!"
                 ,"Coming in hot."
                 ,"Here come the boomers!"
                 ,"Here's my artillery."
                 ,"The command car is here."
                 ,"Here comes the brass."
                 ,"Trains inbound!"
                 ,"Double time."
                 ,"Clear the tracks!"
                 ,"Let's step up the pace!"
                 ,"You're just in time for the rush!"
                 ,"I'm not impressed - more trains are inbound!"
                 ,"I have a schedule to keep!"
                 ,"You're running out of time."
                 ]
        quoteThogar 0 = sendMessage $ Message 0 vid "That wasn't on the... schedule..." [] []
        quoteThogar i = do
            r <- liftIO $ randomRIO (1,length quotes)
            sendMessage $ Message 0 vid (quotes !! r) [] []
            liftIO $ threadDelay 5000000
            quoteThogar (i-1)
    sendMessage $ Message 0 vid "Blackhand won't tolerate anymore delays." [] []
    liftIO $ threadDelay 2000000
    quoteThogar 10

execute vid Status = do
    (UserPermissions perms) <- userperm <$> get
    sendMessage $ Message 0 vid (show $ Map.findWithDefault User (userID vid) perms) [] []

execute vid _ = sendMessage $ Message 0 vid "The command is not implemented yet" [] []

getCardImage :: (MonadVK m, MonadCardsDB m) => Bool -> Locale -> Card -> m (String)
getCardImage golden (Locale loc) c = do
    url <- imageURL <$> get
    let path = if golden then "/animated/" else "/original/"
        ext = if golden then "_premium.gif" else ".png"
        imgurl = url ++ map toLower loc ++ path ++ cardID c ++ ext
    acid <- liftIO $ openLocalState (CardPics Map.empty)
    mbPic <- liftIO $ query acid (GetPic imgurl)
    pid <- case if isNotFound c then Just "" else mbPic of
        Nothing -> do
            r <- liftIO $ ((Right <$> W.get imgurl) `E.catch` \(e :: E.SomeException) -> return (Left e))
            case r of
                Right r' -> do
                    pid <- if golden then uploadDocument "card.gif" (r' ^. W.responseBody) else uploadPhoto (r' ^. W.responseBody)
                    unless (null pid) . liftIO $ update acid (InsertPic imgurl pid)
                    return pid
                Left e -> do
                    liftIO . infoM rootLoggerName $ "No image for " ++ unlocalize (Locale "enUS") (name c) ++ " found"
                    return ""
        Just pid -> do
            return pid
    liftIO $ closeAcidState acid
    return pid

downloadFile :: String -> IO (Either E.SomeException BS.ByteString)
downloadFile url = do
    infoM rootLoggerName $ "Downloading " ++ url
    r <- (Right <$> W.get url) `E.catch` \(e :: E.SomeException) ->  return (Left e)
    return (fmap (\x -> x ^. W.responseBody) r)

getCardSound :: (MonadVK m, MonadCardsDB m) => Locale -> SoundType -> Card -> m (String)
getCardSound (Locale loc) st c = do
    let audiouid = show st ++ loc ++ cardID c
    acid <- liftIO $ openLocalState (AudioIds Map.empty)
    mbAudio <- liftIO $ query acid (GetAudio audiouid)
    pid <- case if isNotFound c then Just "" else mbAudio of
        Just x -> return x
        Nothing -> do
            url <- soundURL <$> get
            let filenames :: [String]
                filenames = do
                    number <- [1..9]
                    soundtype <- [map toUpper (show st), show st]
                    return ("VO_" ++ cardID c ++ "_" ++ soundtype ++ "_0" ++ show number ++ ".mp3")
                audiourls = map (\x -> url ++ (if loc == "enUS" then "" else map toLower loc ++ "/") ++ x) filenames
                downloader :: [String] -> IO (Maybe BS.ByteString)
                downloader [] = return Nothing
                downloader (fname:fnames) = do
                    x <- downloadFile fname
                    case x of
                        Left e -> downloader fnames
                        Right res -> return $ Just res
            d <- liftIO $ downloader audiourls
            p <- case d of
                Nothing -> do
                    liftIO . infoM rootLoggerName $ "No sound file for " ++ unlocalize (Locale loc) (name c) ++ ":" ++ show st ++ " found"
                    return ""
                Just rawdata -> uploadAudio (unlocalize (Locale loc) $ name c) (show st) rawdata
            unless (null p) . liftIO $ update acid (InsertAudio audiouid p)
            return p
    liftIO $ closeAcidState acid
    return pid
