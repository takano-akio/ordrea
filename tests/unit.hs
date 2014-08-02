import Control.Applicative
import Control.Monad
import Data.List.Split
import System.Process

main :: IO ()
main = do
  files <- endBy "\0" <$> readProcess
    "find"
    [ "."
    , "-name", "*.hs"
    , "-exec", "grep", "-q", "^_unitTest", "{}", ";"
    , "-print0"
    ] ""
  forM_ files $ \file -> do
    putStrLn $ "Running unit tests in " ++ file
    rawSystem "ghc"
      [ "-e", "_unitTest"
      , file
      ]
    putChar '\n'
