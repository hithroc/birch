module Card (module X, searchBy, MonadCardsDB, printCards, priority) where

import Card.Parser as X
import Card.Type as X
import Card.Json as X
import Data.Char
import Data.List
import Control.Monad.Ether.Implicit
import qualified Data.Map as Map

type Cards = Map.Map String [Card]

printCards :: Cards -> String
printCards cs = unlines . map printCard . concatMap snd $ Map.toList cs

type MonadCardsDB = MonadReader Cards

searchBy' :: (Card -> String) -> String -> [Card] -> [Card]
searchBy' f n = filter $ \c -> map toUpper n `isInfixOf` map toUpper (f c)

searchBy :: MonadCardsDB m => (Card -> String) -> String -> m Cards
searchBy f n = do
    cards <- ask
    return . Map.map (searchBy' f n) $ cards

priority :: [a] -> [a -> Bool] -> a
priority xs [] = head xs
priority xs (p:ps) = if null f then priority xs ps else priority f ps
    where
        f = filter p xs

