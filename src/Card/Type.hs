{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Card.Type where

import Data.Aeson
import Data.Maybe
import Text.Read
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap

data Locale = Locale String
            | Unknown
    deriving (Read, Show, Eq, Ord)

data Localized = Localized (Map.Map Locale String)
    deriving (Read, Show, Eq, Ord)

data SoundType
    = Play
    | Attack
    | Death
    deriving (Ord, Read, Show, Eq)

data Rarity
    = Free
    | Token
    | Common
    | Rare
    | Epic
    | Legendary
    deriving (Read, Show, Eq)
data Race
    = Mech
    | Murloc
    | Demon
    | Totem
    | Beast
    | Pirate
    | Dragon
    deriving (Read, Show, Eq)
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
    deriving (Read, Show, Eq)

data Card
    = Minion
        { cardID :: String
        , name :: Localized
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , collectible :: Bool
        , cost :: Int
        , attack :: Int
        , health :: Int
        , race :: Maybe Race
        , text :: Maybe Localized
        , flavor :: Maybe Localized
        }
    | Spell
        { cardID :: String
        , name :: Localized
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , collectible :: Bool
        , cost :: Int
        , text :: Maybe Localized
        , flavor :: Maybe Localized
        }
    | HeroPower
        { cardID :: String
        , name :: Localized
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , collectible :: Bool
        , hpCost :: Maybe Int
        , text :: Maybe Localized
        }
    | Hero
        { cardID :: String
        , name :: Localized
        , rarity :: Rarity
        , playerClass :: PlayerClass
        , collectible :: Bool
        , health :: Int
        , race :: Maybe Race
        , text :: Maybe Localized
        , flavor :: Maybe Localized
        }
    | Weapon
        { cardID :: String
        , name :: Localized
        , playerClass :: PlayerClass
        , collectible :: Bool
        , rarity :: Rarity
        , cost :: Int
        , durability :: Int
        , attack :: Int
        , text :: Maybe Localized
        , flavor :: Maybe Localized
        }
    | Enchantment
        { cardID :: String
        }
    | NotFound
        { cardID :: String
        , name :: Localized
        , playerClass :: PlayerClass
        , collectible :: Bool
        , rarity :: Rarity
        }
    deriving Show

notFoundCard :: Card
notFoundCard = NotFound "" (Localized $ Map.empty) Neutral False Free

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

isNotFound :: Card -> Bool
isNotFound (NotFound {}) = True
isNotFound _ = False

unlocalize :: Locale -> Localized -> String
unlocalize l (Localized v) = case Map.lookup l v of
    Just v' -> v'
    Nothing -> case Map.lookup (Locale "enUS") v of
        Just v' -> v'
        Nothing -> "Unknown Locale Error"

printCard :: Locale -> Card -> String
printCard l card@(Minion {}) = ""
    ++ unlocalize l (name card) ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " Minion"
    ++ "\n(" ++ show (cost card) ++ ") " ++ show (attack card) ++ "/" ++ show (health card)
    ++ maybe "" (("\n"++) . show) (race card)
    ++ maybe "" (("\n"++).unlocalize l) (text card)

printCard l card@(Spell {}) = ""
    ++ unlocalize l (name card) ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " Spell"
    ++ "\n(" ++ show (cost card) ++ ")"
    ++ maybe "" (("\n"++).unlocalize l) (text card)

printCard l card@(HeroPower {}) = ""
    ++ unlocalize l (name card) ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " Hero Power"
    ++ "\n(" ++ maybe "No cost" show (hpCost card) ++ ")"
    ++ maybe "" (("\n"++).unlocalize l) (text card)

printCard l card@(Hero {}) = ""
    ++ unlocalize l (name card) ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card) ++ " Hero"
    ++ "\nHP: " ++ show (health card)
    ++ maybe "" (("\n"++) . show) (race card)
    ++ maybe "" (("\n"++).unlocalize l) (text card)

printCard l card@(Weapon {}) = ""
    ++ unlocalize l (name card) ++ " - " ++ show (rarity card) ++ " " ++ show (playerClass card)  ++ " Weapon"
    ++ "\n(" ++ show (cost card) ++ ") " ++ show (attack card) ++ "/" ++ show (durability card)
    ++ maybe "" (("\n"++).unlocalize l) (text card)

printCard l card@(NotFound {}) = "No card named \"" ++ unlocalize l (name card) ++ "\" found"

printCard _ _ = "Unsupported type of card"

instance FromJSON Localized where
    parseJSON (Object v) = do
        let l = HashMap.toList v
        let parseLoc (k,av) = do
                (v' :: String) <- parseJSON av
                return (Locale $ T.unpack k, v')
        fmap (Localized . Map.fromList) $ traverse (parseLoc) l
    parseJSON _ = mzero
            
        
instance FromJSON Card where
    parseJSON (Object v) = do
        cid      <- v .: "id"
        (cardType :: String) <- v .: "type"
        name'    <- v .: "name"
        rarity'  <- v .:? "rarity"
        playerClass' <- v .:? "playerClass"
        text' <- v .:? "text"
        flavor' <- v .:? "flavor"
        collectible' <- v .:? "collectible"
        let playerClass'' = fromMaybe Neutral (playerClass' >>= readMaybe)
            rarity'' = fromMaybe Free (maybe (Just Token) readMaybe rarity')
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
