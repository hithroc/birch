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

type Cards = Map.Map String [Card]

printCards :: Cards -> String
printCards cs = unlines . map printCard . concatMap snd $ Map.toList cs

type MonadCardsDB = MonadReader Cards

searchBy' :: (Card -> String) -> String -> [Card] -> [Card]
searchBy' f n = filter $ \c -> map toUpper n `isInfixOf` map toUpper (f c)

exactSearchBy' :: (Card -> String) -> String -> [Card] -> [Card]
exactSearchBy' f n = filter $ \c -> n == (f c)

searchBy :: MonadCardsDB m => (Card -> String) -> String -> m Cards
searchBy f n = do
    cards <- ask
    return . Map.map (searchBy' f n) $ cards

exactSearchBy :: MonadCardsDB m => (Card -> String) -> String -> m Cards
exactSearchBy f n = do
    cards <- ask
    return . Map.map (exactSearchBy' f n) $ cards

priority :: [a] -> [a -> Bool] -> a
priority xs [] = head xs
priority xs (p:ps) = if null f then priority xs ps else priority f ps
    where
        f = filter p xs

processTag :: MonadCardsDB m => CardTag -> Card -> m Card
processTag (Loc (Locale l)) c = do
    cards <- exactSearchBy cardID $ cardID c
    case Map.lookup l cards >>= listToMaybe of
        Nothing -> return c
        Just c' -> return c'
processTag _ c = return c

processTags :: MonadCardsDB m => [CardTag] -> Card -> m Card
processTags tags card = foldl (\a b -> a >>= processTag b) (return card) tags

processCard :: (MonadCardsDB m, MonadIO m) => [Card -> Bool] -> (S.Set CardTag, String) -> m (S.Set CardTag, Card)
processCard prio (tags, n) = do
    fc <- searchBy name n
    let cards = concatMap snd . Map.toList $ fc
    if null cards then
        return $ (tags, notFoundCard { name = n })
    else do
        let resultcard = priority cards prio
        (\x -> (tags, x)) <$> processTags (S.toList tags) resultcard
