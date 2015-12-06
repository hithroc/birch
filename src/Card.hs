module Card (module Card.Parser, module Card.Type, module Card.Json) where

import Card.Parser
import Card.Type
import Card.Json
import Data.Char
import Data.List

searchByName :: String -> [Card] -> [Card]
searchByName n = filter (\c -> isInfixOf (map toUpper n) (map toUpper $ name c))
