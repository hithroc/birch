{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Card.Type where

import Data.Aeson
import Data.Maybe
import Text.Read
import Control.Monad

data Rarity
    = Free
    | Common
    | Rare
    | Epic
    | Legendary
    deriving (Read, Show)
data Race
    = Mech
    | Murloc
    | Demon
    | Totem
    | Beast
    | Pirate
    | Dragon
    deriving (Read, Show)
data PlayerClass
    = Neutral
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

data Card
    = Minion
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , cost :: Int
        , attack :: Int
        , health :: Int
        , race :: Maybe Race
        , text :: Maybe String
        , flavor :: Maybe String
        }
    | Spell
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , cost :: Int
        , text :: Maybe String
        , flavor :: Maybe String
        }
    | HeroPower
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , hpCost :: Maybe Int
        , text :: Maybe String
        }
    | Hero
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , health :: Int
        , race :: Maybe Race
        , text :: Maybe String
        , flavor :: Maybe String
        }
    | Weapon
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , cost :: Int
        , playerClass :: PlayerClass
        , durability :: Int
        , attack :: Int
        , text :: Maybe String
        , flavor :: Maybe String
        }
    | Enchantment
        { cardID :: String
        }
    deriving Show

printCard :: Card -> String
printCard card@(Minion {}) = ""
    ++ name card ++ " - " ++ show (playerClass card) ++ " " ++ show (rarity card) ++ " Minion"
    ++ "\n(" ++ show (cost card) ++ ") " ++ show (attack card) ++ "/" ++ show (health card)
    ++ maybe "" (("\n"++) . show) (race card)
    ++ maybe "" ("\n"++) (text card)

printCard card@(Spell {}) = ""
    ++ name card ++ " - " ++ show (playerClass card) ++ " " ++ show (rarity card) ++ " Spell"
    ++ "\n(" ++ show (cost card) ++ ")"
    ++ maybe "" ("\n"++) (text card)

printCard card@(HeroPower {}) = ""
    ++ name card ++ " - " ++ show (playerClass card) ++ " " ++ show (rarity card) ++ " Hero Power"
    ++ "\n(" ++ maybe "No cost" show (hpCost card) ++ ")"
    ++ maybe "" ("\n"++) (text card)

printCard card@(Hero {}) = ""
    ++ name card ++ " - " ++ show (playerClass card) ++ " " ++ show (rarity card) ++ " Hero"
    ++ "\nHP: " ++ show (health card)
    ++ maybe "" (("\n"++) . show) (race card)
    ++ maybe "" ("\n"++) (text card)

printCard card@(Weapon {}) = ""
    ++ name card ++ " - " ++ show (playerClass card) ++ " " ++ show (rarity card) ++ " Weapon"
    ++ "\n(" ++ show (cost card) ++ ") " ++ show (attack card) ++ "/" ++ show (durability card)
    ++ maybe "" ("\n"++) (text card)

printCard _ = "Unsupported type of card"

priority :: [a] -> [a -> Bool] -> a
priority xs [] = head xs
priority xs (p:ps) = if null f then priority xs ps else priority f ps
    where
        f = filter p xs

instance FromJSON Card where
    parseJSON (Object v) = do
        cid      <- v .: "id"
        (cardType :: String) <- v .: "type"
        name'    <- v .: "name"
        rarity'  <- v .: "rarity"
        playerClass' <- v .:? "playerClass"
        text' <- v .:? "text"
        flavor' <- v .:? "flavor"
        let playerClass'' = fromMaybe Neutral (playerClass' >>= readMaybe)
            rarity'' = fromMaybe Free (readMaybe rarity')
        case cardType of
            "Minion" -> do
                cost' <- v .: "cost"
                attack' <- v .: "attack"
                health' <- v .: "health"
                race' <- v .:? "race"
                return Minion
                    { cardID = cid
                    , name = name'
                    , rarity = rarity''
                    , cost = cost'
                    , attack = attack'
                    , health = health'
                    , race = race' >>= readMaybe
                    , playerClass = playerClass''
                    , text = text'
                    , flavor = flavor'
                    }
            "Spell" -> do
                cost' <- v .: "cost"
                return Spell
                    { cardID = cid
                    , name = name'
                    , rarity = rarity''
                    , cost = cost'
                    , playerClass = playerClass''
                    , text = text'
                    , flavor = flavor'
                    }
            "Hero Power" -> do
                cost' <- v .:? "cost"
                return HeroPower
                    { cardID = cid
                    , name = name'
                    , rarity = rarity''
                    , hpCost = cost'
                    , playerClass = playerClass''
                    , text = text'
                    }
            "Hero" -> do
                health' <- v .: "health"
                race' <- v .:? "race"
                return Hero
                    { cardID = cid
                    , name = name'
                    , health = health'
                    , rarity = rarity''
                    , race = race' >>= readMaybe
                    , playerClass = playerClass''
                    , text = text'
                    , flavor = flavor'
                    }
            "Weapon" -> do
                durability' <- v .: "durability"
                attack' <- v .: "attack"
                cost' <- v .: "cost"
                return Weapon
                    { cardID = cid
                    , name = name'
                    , cost = cost'
                    , durability = durability'
                    , attack = attack'
                    , rarity = rarity''
                    , playerClass = playerClass''
                    , text = text'
                    , flavor = flavor'
                    }
            _ -> mzero
