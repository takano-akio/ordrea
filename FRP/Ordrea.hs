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
  , delayForE, throttleE, debounceE

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
import Data.AffineSpace (AffineSpace(..), (.-^))
import Data.Foldable (foldl', toList)
import Data.Monoid (Last(..), (<>))
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

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
    countdown occ k = (k', (k, occ))
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

-- | Delay the event for the given period.
delayForE
  :: (AffineSpace time, Ord time)
  => Diff time
  -- ^ Delay period
  -> Behavior time
  -- ^ Current time
  -> Event a
  -> SignalGen (Event a)
delayForE duration nowB evt = do
  expiredE <- mapAccumE Seq.empty $ updateBuffer
    <$> eventToBehavior evt
    <@> nowB <@ stepClockE
  return $ flattenE $ map snd . toList <$> expiredE
  where
    updateBuffer occs now = validate now . addToBuffer occs now
    addToBuffer occs now buffer =
      foldl' (\buf val -> buf |> (now, val)) buffer occs
    validate now buffer = (newBuffer, expired)
      where
        (expired, newBuffer) = Seq.spanl (isExpired . fst) buffer
        isExpired time = time <= now .-^ duration

-- | Ensure that the event is never triggered more frequently than the
-- specified period. If there are some subsequent occurrences in the
-- period, they are just ignored.
throttleE
  :: (AffineSpace time, Ord time)
  => Diff time
  -- ^ Throttling period
  -> Behavior time
  -- ^ Current time
  -> Event a
  -> SignalGen (Event a)
throttleE duration nowB evt = do
  throttledE <- mapAccumE Nothing $ f <$> nowB <@> evt
  return $ justE throttledE
  where
    f now val Nothing = (Just now, Just val)
    f now val (Just prev)
      | prev <= now .-^ duration = (Just now, Just val)
      | otherwise = (Just prev, Nothing)

-- | Delay the event until the debounce period has elapsed with no subsequent
-- events are triggered. If any events are triggered before the specified time
-- has elapsed, the timer is reset and entire period must pass again.
-- Otherwise, the last triggered event will be returned.
debounceE
  :: (AffineSpace time, Ord time)
  => Diff time
  -- ^ Debouncing period
  -> Behavior time
  -- ^ Current time
  -> Event a
  -> SignalGen (Event a)
debounceE duration nowB evt = do
  delayedE <- delayForE duration nowB evt
  let
    enqueueE = enqueue <$> evt
    dequeueE = dequeue <$ delayedE
  queueD <- scanD (Last Nothing, 0 :: Int) $ enqueueE <> dequeueE
  let emitE = triggerWhen $ (== 0) . snd <$> queueD
  return $ justE $ getLast . fst <$> queueD <@ emitE
  where
    enqueue val (lastVal, n) = (lastVal', n')
      where
        !lastVal' = lastVal <> Last (Just val)
        !n' = n + 1
    dequeue (lastVal, n)
      | n > 0 = n' `seq` (lastVal, n')
      | otherwise = (lastVal, n)
      where
        n' = n - 1
    triggerWhen dis = justE $ f <$> changesD dis
      where
        f b = if b then Just () else Nothing

----------------------------------------------------------------------
-- tests

_unitTest = runTestTT $ test
  [ test_withPrevE
  , test_dropE
  , test_dropWhileE
  , test_takeE
  , test_delayForE
  , test_throttleE
  , test_debounceE
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

test_delayForE = do
  r <- networkToList 5 $ do
    timeB <- behaviorFromList [0..9 :: Double]
    evt <- eventFromList $ map (\x -> [x, x+1]) [0..9 :: Int]
    eventToBehavior <$> delayForE 2 timeB evt
  r @?= [[], [], [0,1], [1,2], [2,3]]

test_throttleE = do
  r <- networkToList 10 $ do
    timeB <- behaviorFromList [0..9 :: Double]
    evt <- eventFromList $ map return [0..9 :: Int]
    eventToBehavior <$> throttleE 2.5 timeB evt
  r @?= [[0], [], [], [3], [], [], [6], [], [], [9]]

test_debounceE = do
  r <- networkToList 10 $ do
    timeB <- behaviorFromList [0..9 :: Double]
    evt <- eventFromList [[0 :: Int], [], [], [1], [2], [3], [], [], [], []]
    eventToBehavior <$> debounceE 1.5 timeB evt
  r @?= [[], [], [0], [], [], [], [], [3], [], []]
