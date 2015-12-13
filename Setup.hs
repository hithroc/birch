import Distribution.Simple
import System.Process
main = do
    -- I'm not happy about this
    _ <- readProcess "touch" ["./src/Version/TH.hs"] ""
    defaultMain
