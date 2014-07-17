{-# LANGUAGE CPP #-}

module UnitTest
  ( (@?=)
  , runTestTT
  , Counts
  , Test
  , Assertion
  , test
  ) where

#ifdef NOTEST

(@?=) :: a -> a -> IO ()
(@?=) = undefined
runTestTT :: a
runTestTT = undefined
type Counts = ()
type Test = ()
type Assertion = IO ()
test :: a
test = undefined

#else

import Test.HUnit

#endif

