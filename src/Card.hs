module Card (module X, searchBy, exactSearchBy, MonadCardsDB, printCards, priority, processCard, Cards) where

import Card.Parser as X
import Card.Type as X
import Card.Json as X
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Ether.Implicit
import qualified Data.Map as Map
import qualified Data.Set as S

type Cards = [Card]

priority :: [a -> Bool] -> [a] -> a
priority [] xs = head xs
priority (p:ps) xs = if null f then priority ps xs else priority ps f
    where
        f = filter p xs

printCards :: Locale -> Cards -> String
printCards l cs = unlines . map (printCard l) $ cs

type MonadCardsDB = MonadReader Cards

searchBy' :: (Card -> String) -> String -> [Card] -> [Card]
searchBy' f n = filter $ \c -> map toUpper n `isInfixOf` map toUpper (f c)

searchLocalized' :: (Card -> Localized) -> String -> [Card] -> [(Locale, Card)]
searchLocalized' f n cards = matching
    where
        loclist c = Map.toList . locToMap $ f c
        comp x = map toUpper n `isInfixOf` map toUpper x
        complocs :: [(Locale, String)] -> [Bool]
        complocs = map (\(loc, n') -> comp n')
        prio = priority [(==(Locale "ruRU")),(==(Locale "enUS"))] . map snd
        matching :: [(Locale, Card)]
        matching = map (\(x,y) -> (prio $ filter fst (zip (complocs x) (map fst x)), y)) 
                 . filter (\(x, _) -> or $ complocs x) 
                 . map (\c -> (loclist c, c)) 
                 $ cards

exactSearchBy' :: (Card -> String) -> String -> [Card] -> [Card]
exactSearchBy' f n = filter $ \c -> n == (f c)

searchBy :: MonadCardsDB m => (Card -> String) -> String -> m Cards
searchBy f n = do
    cards <- ask
    return . searchBy' f n $ cards

searchLocalized :: MonadCardsDB m => (Card -> Localized) -> String -> m [(Locale, Card)]
searchLocalized f n = do
    cards <- ask
    return . searchLocalized' f n $ cards

exactSearchBy :: MonadCardsDB m => (Card -> String) -> String -> m Cards
exactSearchBy f n = do
    cards <- ask
    return . exactSearchBy' f n $ cards


processTag :: MonadCardsDB m => CardTag -> (Locale, Card) -> m (Locale, Card)
processTag (Loc l) (_,c) = return (l,c)
processTag _ x = return x

processTags :: MonadCardsDB m => [CardTag] -> (Locale, Card) -> m (Locale, Card)
processTags tags card = foldl (\a b -> a >>= processTag b) (return card) tags

processCard :: (MonadCardsDB m, MonadIO m) => [Card -> Bool] -> (S.Set CardTag, String) -> m (S.Set CardTag, (Locale, Card))
processCard prio (tags, n) = do
    cards <- searchLocalized name n
    if null cards then
        return $ (tags, (Locale "enUS", notFoundCard { name = Localized $ Map.singleton (Locale "enUS") n }))
    else do
        let resultcard = priority (map (.snd) prio) cards
        (\x -> (tags, x)) <$> processTags (S.toList tags) resultcard
