{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module AudioDB where

import Data.Acid
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Data.SafeCopy
import qualified Data.Map as Map

type AudioUID = String
type AudioVKID = String
data AudioIds = AudioIds !(Map.Map AudioUID AudioVKID)
    deriving Typeable

$(deriveSafeCopy 0 'base ''AudioIds)

insertAudio :: AudioUID -> AudioVKID -> Update AudioIds ()
insertAudio uid vkid = do
    AudioIds m <- get
    put (AudioIds (Map.insert uid vkid m))

getAudio :: AudioUID -> Query AudioIds (Maybe AudioVKID)
getAudio uid = do
    AudioIds m <- ask
    return (Map.lookup uid m)

$(makeAcidic ''AudioIds ['insertAudio, 'getAudio])
