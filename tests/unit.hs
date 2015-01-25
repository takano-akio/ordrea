import Control.Applicative
import Control.Monad
import Data.List.Split
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  files <- endBy "\0" <$> readProcess
    "find"
    [ "."
    , "-name", "*.hs"
    , "-exec", "grep", "-q", "^_unitTest", "{}", ";"
    , "-print0"
    ] ""
  forM_ files $ \file -> do
    putStrLn $ "Running unit tests in " ++ file
    code <- rawSystem "ghc"
      [ "-e", "_unitTest"
      , file
      ]
    when (code /= ExitSuccess) $ exitWith code
    putChar '\n'
