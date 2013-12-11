import qualified Tests.Base as Base

import Control.Monad
import Test.HUnit
import System.Exit

main :: IO ()
main = do
  c <- runTestTT tests
  print c
  when ((errors c, failures c) /= (0, 0))
    exitFailure

tests :: Test
tests = test
  [ "base" ~: Base.tests
  ]
