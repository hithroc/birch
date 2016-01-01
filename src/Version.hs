{-# LANGUAGE TemplateHaskell #-}
module Version where
import Version.TH
import Data.Version (showVersion)
import qualified Paths_birch as P

version :: String
version = showVersion (P.version) ++ "-" ++ $(getCommitHash)
