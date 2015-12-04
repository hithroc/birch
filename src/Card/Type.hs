{-# LANGUAGE OverloadedStrings #-}
module Card.Type where

import Data.Aeson
import Data.Maybe

data CardType = Minion
              | Spell
              | HeroPower
              | Hero
              | Weapon
              | Enchantment
              | Unknown
    deriving (Read, Show, Eq)
data Rarity = Free
            | Common
            | Rare
            | Epic
            | Legendary
    deriving (Read, Show)
data Race = Mech
          | Murloc
          | Demon
          | Totem
          | Beast
          | Pirate
          | Dragon
    deriving (Read, Show)
data PlayerClass = Neutral
                 | Druid
                 | Hunter
                 | Mage
                 | Paladin
                 | Priest
                 | Rogue
                 | Shaman
                 | Warrior
                 | Warlock
                 | Dream
    deriving (Read, Show)

data Card = Card { cardID :: String
                 , name :: String
                 , cost :: Maybe Int
                 , attack :: Maybe Int
                 , health :: Maybe Int
                 , durability :: Maybe Int
                 , rarity :: Rarity
                 , cardType :: CardType
                 , race :: Maybe Race
                 , playerClass :: PlayerClass
                 , text :: Maybe String
                 , flavor :: Maybe String
                 }

printCard :: Card -> String
printCard card
    | cardType card == Minion =
        name card ++ " - " 
        ++ show (rarity card) 
        ++ " " 
        ++ show (cardType card)
        ++ " "
        ++ show (playerClass card)
        ++ "\n"
        ++ "(" ++ show (maybe 0 id $ cost card) ++ ") "
        ++ show (maybe 0 id $ attack card)
        ++ "/"
        ++ show (maybe 0 id $ health card)
        ++ maybe "" ((" - " ++) . show) (race card)
        ++ "\n"
        ++ maybe "" (++"\n") (text card)
        ++ maybe "" (++"\n") (flavor card)
    | cardType card == Spell =
        name card ++ " - " 
        ++ show (rarity card) 
        ++ " " 
        ++ show (cardType card)
        ++ " - "
        ++ show (playerClass card)
        ++ "\n"
        ++ "(" ++ show (maybe 0 id $ cost card) ++ ") "
        ++ "\n"
        ++ maybe "" (++"\n") (text card)
        ++ maybe "" (++"\n") (flavor card)
    | cardType card == Hero =
        name card ++ " - " 
        ++ show (rarity card) 
        ++ " " 
        ++ show (cardType card)
        ++ " - "
        ++ show (playerClass card)
        ++ "\n"
        ++ "HP:" ++ show (maybe 0 id $ health card)
        ++ maybe "" ((" - " ++) . show) (race card)
        ++ "\n"
        ++ maybe "" (++"\n") (text card)
        ++ maybe "" (++"\n") (flavor card)
    | cardType card == HeroPower =
        name card ++ " - " 
        ++ show (rarity card) 
        ++ " " 
        ++ show (cardType card)
        ++ " - "
        ++ show (playerClass card)
        ++ "\n"
        ++ "(" ++ show (maybe 0 id $ cost card) ++ ") "
        ++ "\n"
        ++ maybe "" (++"\n") (text card)
        ++ maybe "" (++"\n") (flavor card)
    | cardType card == Weapon =
        name card ++ " - " 
        ++ show (rarity card) 
        ++ " " 
        ++ show (cardType card)
        ++ " - "
        ++ show (playerClass card)
        ++ "\n"
        ++ "(" ++ show (maybe 0 id $ cost card) ++ ") "
        ++ show (maybe 0 id $ attack card)
        ++ "/"
        ++ show (maybe 0 id $ durability card)
        ++ "\n"
        ++ maybe "" (++"\n") (text card)
        ++ maybe "" (++"\n") (flavor card)
    | otherwise = name card ++ " - Not supported"

priority :: [a] -> [a -> Bool] -> a
priority xs [] = head xs
priority xs (p:ps) = if null f then priority xs ps else priority f ps
    where
        f = filter p xs

instance FromJSON Card where
    parseJSON (Object v) = do
        cid          <- v .: "id"
        name'        <- v .: "name"
        rarity'      <- v .: "rarity"
        cardType'    <- v .: "type"
        playerClass' <- v .:? "playerClass"
        cost'        <- v .:? "cost"
        attack'      <- v .:? "attack"
        health'      <- v .:? "health"
        durability'  <- v .:? "durability"
        race'        <- v .:? "race"
        text'        <- v .:? "text"
        flavor'      <- v .:? "flavor"
        let ct = if cardType' == "Hero Power" then "HeroPower" else cardType'
        return Card { cardID = cid
                    , name = name'
                    , rarity = read rarity'
                    , cardType = read ct
                    , playerClass = maybe Neutral read playerClass'
                    , cost = cost'
                    , attack = attack'
                    , health = health'
                    , durability = durability'
                    , race = fmap read race'
                    , text = text'
                    , flavor = flavor'
                    }
