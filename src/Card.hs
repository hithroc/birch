module Card (module Card.Parser, module Card.Type, module Card.Json, searchCards) where

import Card.Parser
import Card.Type
import Card.Json
import Data.Char
import Data.List

searchByName :: [Card] -> String -> [Card]
searchByName cards n = filter (\c -> isInfixOf (map toUpper n) (map toUpper $ name c)) cards

searchCards :: [Card] -> [String] -> [Card]
searchCards cards = concat . map (searchByName cards)
