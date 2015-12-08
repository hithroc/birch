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
        , collectible :: Bool
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
        , collectible :: Bool
        , cost :: Int
        , text :: Maybe String
        , flavor :: Maybe String
        }
    | HeroPower
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , collectible :: Bool
        , hpCost :: Maybe Int
        , text :: Maybe String
        }
    | Hero
        { cardID :: String
        , name :: String
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , collectible :: Bool
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
        , collectible :: Bool
        , durability :: Int
        , attack :: Int
        , text :: Maybe String
        , flavor :: Maybe String
        }
    | Enchantment
        { cardID :: String
        }
    deriving Show

isMinion :: Card -> Bool
isMinion (Minion {}) = True
isMinion _ = False

isSpell :: Card -> Bool
isSpell (Spell {}) = True
isSpell _ = False

isHeroPower :: Card -> Bool
isHeroPower (HeroPower {}) = True
isHeroPower _ = False

isHero :: Card -> Bool
isHero (Hero {}) = True
isHero _ = False

isWeapon :: Card -> Bool
isWeapon (Weapon {}) = True
isWeapon _ = False

isEnchantment :: Card -> Bool
isEnchantment (Enchantment {}) = True
isEnchantment _ = False

printCard :: Card -> String
printCard card@(Minion {}) = ""
    ++ name card ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " " ++ " Minion"
    ++ "\n(" ++ show (cost card) ++ ") " ++ show (attack card) ++ "/" ++ show (health card)
    ++ maybe "" (("\n"++) . show) (race card)
    ++ maybe "" ("\n"++) (text card)

printCard card@(Spell {}) = ""
    ++ name card ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " " ++ " Spell"
    ++ "\n(" ++ show (cost card) ++ ")"
    ++ maybe "" ("\n"++) (text card)

printCard card@(HeroPower {}) = ""
    ++ name card ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " " ++ " Hero Power"
    ++ "\n(" ++ maybe "No cost" show (hpCost card) ++ ")"
    ++ maybe "" ("\n"++) (text card)

printCard card@(Hero {}) = ""
    ++ name card ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " " ++ " Hero"
    ++ "\nHP: " ++ show (health card)
    ++ maybe "" (("\n"++) . show) (race card)
    ++ maybe "" ("\n"++) (text card)

printCard card@(Weapon {}) = ""
    ++ name card ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " " ++ " Weapon"
    ++ "\n(" ++ show (cost card) ++ ") " ++ show (attack card) ++ "/" ++ show (durability card)
    ++ maybe "" ("\n"++) (text card)

printCard _ = "Unsupported type of card"

instance FromJSON Card where
    parseJSON (Object v) = do
        cid      <- v .: "id"
        (cardType :: String) <- v .: "type"
        name'    <- v .: "name"
        rarity'  <- v .: "rarity"
        playerClass' <- v .:? "playerClass"
        text' <- v .:? "text"
        flavor' <- v .:? "flavor"
        collectible' <- v .:? "collectible"
        let playerClass'' = fromMaybe Neutral (playerClass' >>= readMaybe)
            rarity'' = fromMaybe Free (readMaybe rarity')
            collectible'' = maybe False id collectible'
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
                    , collectible = collectible''
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
                    , collectible = collectible''
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
                    , collectible = collectible''
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
                    , collectible = collectible''
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
                    , collectible = collectible''
                    }
            _ -> mzero
    parseJSON _ = mzero
