module Card (module X, searchBy, MonadCardsDB) where

import Card.Parser as X
import Card.Type as X
import Card.Json as X
import Data.Char
import Data.List
import Control.Monad.Ether.Implicit
import qualified Data.Map as Map

type Cards = Map.Map String [Card]

type MonadCardsDB = MonadReader Cards

searchBy' :: (Card -> String) -> String -> [Card] -> [Card]
searchBy' f n = filter $ \c -> map toUpper n `isInfixOf` map toUpper (f c)

searchBy :: MonadCardsDB m => (Card -> String) -> String -> m Cards
searchBy f name = do
    cards <- ask
    return . Map.map (searchBy' f name) $ cards

