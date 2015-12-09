{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module CardPictureDB where

import Data.Acid
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Data.SafeCopy
import qualified Data.Map as Map

type CardURL = String
type PhotoID = String
data CardPics = CardPics !(Map.Map CardURL PhotoID)
    deriving Typeable

$(deriveSafeCopy 0 'base ''CardPics)

insertPic :: CardURL -> PhotoID -> Update CardPics ()
insertPic curl pid = do
    CardPics m <- get
    put (CardPics (Map.insert curl pid m))

getPic :: CardURL -> Query CardPics (Maybe PhotoID)
getPic curl = do
    CardPics m <- ask
    return (Map.lookup curl m)

$(makeAcidic ''CardPics ['insertPic, 'getPic])
