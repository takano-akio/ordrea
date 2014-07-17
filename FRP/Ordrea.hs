{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module FRP.Ordrea
  (
  -- * Basic types
    SignalGen
  , Behavior, Event, Discrete

  -- * External events
  , ExternalEvent
  , newExternalEvent, triggerExternalEvent

  -- * Events
  , generatorE, filterE, stepClockE, dropStepE, eventFromList
  , scanE, mapAccumE, mapAccumEM
  , accumE, scanAccumE, scanAccumEM
  , mapMaybeE, justE, flattenE, expandE, externalE
  , takeWhileE, delayE
  , withPrevE, dropE, dropWhileE, takeE
  , partitionEithersE, leftsE, rightsE

  -- * Switchers
  , joinDD, joinDE, joinDB

  -- * Behaviors
  , start, externalB, joinB, delayB, behaviorFromList, networkToList
  , networkToListGC

  -- * Discretes
  , scanD, changesD, preservesD, delayD
  , stepperD

  -- * Behavior-event functions
  , eventToBehavior, behaviorToEvent, applyBE

  -- * Behavior-discrete functions
  , discreteToBehavior

  -- * Overloaded functions
  , TimeFunction(..), (<@>), (<@)

  -- * Errors
  , OrderingViolation (..)
  ) where

import Control.Applicative

import FRP.Ordrea.Base
import UnitTest

-- Derived functions

stepperD :: a -> Event a -> SignalGen (Discrete a)
stepperD initial evt = scanD initial (const <$> evt)

withPrevE :: a -> Event a -> SignalGen (Event (a, a))
withPrevE initial evt = scanE (initial, err) $ upd <$> evt
  where
    upd new (old, _) = (new, old)
    err = error "FRP.Ordrea.withPrevE: bug: prehistoric element"

dropE :: Int -> Event a -> SignalGen (Event a)
dropE n evt = justE <$> mapAccumE n (f <$> evt)
  where
    f occ 0 = (0, Just occ)
    f _ k = (k', Nothing)
      where !k' = k - 1

dropWhileE :: (a -> Bool) -> Event a -> SignalGen (Event a)
dropWhileE p evt = justE <$> mapAccumE True (f <$> evt)
  where
    f occ True
      | p occ = (True, Nothing)
    f occ _ = (False, Just occ)

takeE :: Int -> Event a -> SignalGen (Event a)
takeE n evt = do
  evtWithCount <- mapAccumE n (countdown <$> evt)
  fmap snd <$> takeWhileE ((>0) . fst) evtWithCount
  where
    countdown occ k = (k', (k', occ))
      where
        !k' = k - 1

partitionEithersE :: Event (Either a b) -> (Event a, Event b)
partitionEithersE evt = (leftsE evt, rightsE evt)

leftsE :: Event (Either a b) -> Event a
leftsE evt = mapMaybeE f evt
  where
    f (Left x) = Just x
    f Right{} = Nothing

rightsE :: Event (Either a b) -> Event b
rightsE evt = mapMaybeE f evt
  where
    f (Right x) = Just x
    f Left{} = Nothing

----------------------------------------------------------------------
-- tests

_unitTest = runTestTT $ test
  [ test_withPrevE
  , test_dropE
  , test_dropWhileE
  , test_takeE
  ]

test_withPrevE = do
  r <- networkToList 3 $ do
    evt <- withPrevE 0 =<< eventFromList [[1,2], [], [3 :: Int]]
    return $ eventToBehavior evt
  r @?= [[(1,0), (2,1)], [], [(3,2)]]

test_dropE = do
  r <- networkToList 3 $ do
    evt <- dropE 1 =<< eventFromList [[1,2], [], [3 :: Int]]
    return $ eventToBehavior evt
  r @?= [[2], [], [3]]

test_dropWhileE = do
  r <- networkToList 3 $ do
    evt <- dropWhileE (<=2)
      =<< eventFromList [[1,2], [], [3,4 :: Int]]
    return $ eventToBehavior evt
  r @?= [[], [], [3,4]]

test_takeE = do
  r <- networkToList 3 $ do
    evt <- takeE 3
      =<< eventFromList [[1,2], [], [3,4 :: Int]]
    return $ eventToBehavior evt
  r @?= [[1,2], [], [3]]
