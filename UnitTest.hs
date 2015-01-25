{-# LANGUAGE CPP #-}

module UnitTest
  ( (@?=)
  , runTestThrow
  , Counts
  , Test
  , Assertion
  , test
  ) where

#ifdef NOTEST

(@?=) :: a -> a -> IO ()
(@?=) = undefined
runTestThrow :: a
runTestThrow = undefined
type Counts = ()
type Test = ()
type Assertion = IO ()
test :: a
test = undefined

#else

import Control.Monad
import Test.HUnit

runTestThrow :: Test -> IO ()
runTestThrow t = do
  counts <- runTestTT t
  when (errors counts /= 0 || failures counts /= 0) $
    fail "test failure"

#endif

