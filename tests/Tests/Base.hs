{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Unit tests for FRP.Ordrea.Base
module Tests.Base (tests) where

import Control.Applicative
import Control.Exception
import Control.Monad.Fix
import qualified Data.Char as Char
import Data.Monoid
import System.IO.Unsafe
import System.Mem
import System.Mem.Weak
import Test.HUnit

import FRP.Ordrea.Base
import FRP.Ordrea.Weak

tests :: Test
tests = test
  [ test_signalFromList
  , test_signalToEvent
  , test_accumD
  , test_changesD
  , test_delayD
  , test_mappendEvent
  , test_fmapEvent
  , test_filterE
  , test_dropStepE
  , test_dropStepE1
  , test_apDiscrete
  , test_apDiscrete1
  , test_eventFromList
  , test_preservesD
  , test_joinS
  , test_delayS
  , test_generatorE
  , test_generatorE1
  , test_accumE
  , test_fmapSignal
  , test_applySE
  , test_joinDD
  , test_joinDE
  , test_joinDS
  , test_mfix
  , test_orderingViolation_joinDS
  , test_externalEvent
  , test_externalE
  , test_mapAccumE
  , test_mapAccumEM
  , test_mapAccumEquivalent
  , test_delayE
  ]

_skipped =
  [ test_takeWhileE -- broken
  ]

test_signalFromList = do
  r <- networkToList 4 $ signalFromList ["foo", "bar", "baz", "quux", "xyzzy"]
  r @?= ["foo", "bar", "baz", "quux"]

test_signalToEvent = do
  r <- networkToList 3 $ do
    s0 <- signalFromList ["foo", "", "baz"]
    return $ eventToSignal $ signalToEvent s0
  r @?= ["foo", "", "baz"]

test_accumD = do
  r <- networkToList 3 $ do
    strS <- signalFromList ["foo", "", "baz"]
    accD <- accumD "<>" $ append <$> signalToEvent strS
    return $ discreteToSignal accD
  r @?= ["<>/'f'/'o'/'o'", "<>/'f'/'o'/'o'", "<>/'f'/'o'/'o'/'b'/'a'/'z'"]
  where
    append ch str = str ++ "/" ++ show ch

test_changesD = do
  r <- networkToList 3 $ do
    strS <- signalFromList ["foo", "", "baz"]
    accD <- accumD "<>" $ append <$> signalToEvent strS
    return $ eventToSignal $ changesD accD
  r @?= [["<>/'f'/'o'/'o'"], [], ["<>/'f'/'o'/'o'/'b'/'a'/'z'"]]
  where
    append ch str = str ++ "/" ++ show ch

test_delayD = do
  r <- networkToList 5 $ do
    nS <- signalFromList (map pure $ iterate (+1) 0)
    nD <- accumD (0 :: Int) (const <$> signalToEvent nS)
    nD' <- delayD (-1) nD
    nE <- preservesD ((,) <$> nD <*> nD')
    return $ eventToSignal nE
  r @?= map pure [(0, -1), (1, 0), (2, 1), (3, 2), (4, 3)]

test_mappendEvent = do
  r <- networkToListGC 3 $ do
    strS <- signalFromList ["foo", "", "baz"]
    accD <- accumD "<>" $ append <$> signalToEvent strS
    ch <- preservesD accD
    return $ eventToSignal $
      ch `mappend` (signalToEvent $ (:[]) <$> strS)
  r @?= [["<>/'f'/'o'/'o'", "foo"], [""], ["<>/'f'/'o'/'o'/'b'/'a'/'z'", "baz"]]
  where
    append ch str = str ++ "/" ++ show ch

test_fmapEvent = do
  succCountRef <- newRef (0::Int)
  r <- networkToListGC 3 $ do
    strS <- signalFromList ["foo", "", "baz"]
    let lenE = mysucc succCountRef <$> signalToEvent strS
    return $ eventToSignal $ lenE `mappend` lenE
  r @?= ["gppgpp", "", "cb{cb{"]
  count <- readRef succCountRef
  count @?= 6
  where
    {-# NOINLINE mysucc #-}
    mysucc ref c = unsafePerformIO $ do
      modifyRef ref (+1)
      return $ succ c

test_filterE = do
  r <- networkToListGC 4 $ do
    strS <- signalFromList ["FOo", "", "nom", "bAz"]
    let lenE = filterE Char.isUpper $ signalToEvent strS
    return $ eventToSignal $ lenE `mappend` lenE
  r @?= ["FOFO", "", "", "AA"]

test_dropStepE = do
  r <- networkToListGC 3 $ do
    strS <- signalFromList ["foo", "", "baz"]
    lenE <- dropStepE $ signalToEvent strS
    return $ eventToSignal $ lenE `mappend` lenE
  r @?= ["", "", "bazbaz"]

test_dropStepE1 = do
  r <- networkToListGC 3 $
    eventToSignal <$> dropStepE stepClockE
  r @?= [[], [()], [()]]

test_apDiscrete = do
  r <- networkToListGC 4 $ do
    ev0 <- signalToEvent <$> signalFromList [[], [], [1::Int], [2,3]]
    ev1 <- signalToEvent <$> signalFromList [[], [4], [], [5]]
    dis0 <- accumD 0 $ max <$> ev0
    dis1 <- accumD 0 $ max <$> ev1
    let dis = (*) <$> dis0 <*> dis1
    eventToSignal <$> preservesD dis
  r @?= [[0], [0], [4], [15]]

test_apDiscrete1 = do
  r <- networkToListGC 4 $ do
    ev0 <- eventFromList [[], [], [2::Int], [3,4]]
    ev1 <- eventFromList [[-1], [7], [], [11]]
    dis0 <- accumD 1 $ const <$> ev0
    dis1 <- accumD 1 $ const <$> ev1
    let dis = (*) <$> dis0 <*> dis1
    return $ discreteToSignal dis
  r @?= [-1, 7, 14, 44]

test_eventFromList = do
  r <- networkToListGC 3 $ do
    ev <- eventFromList [[2::Int], [], [3,4]]
    return $ eventToSignal ev
  r @?= [[2], [], [3,4]]

test_preservesD = do
  r <- networkToListGC 3 $ do
    ev <- eventFromList [[], [], [3,4::Int]]
    dis <- accumD 0 (const <$> ev)
    ev1 <- preservesD dis
    return $ eventToSignal ev1
  r @?= [[0], [], [4]]

test_joinS = do
  r <- networkToListGC 4 $ do
    sig0 <- signalFromList [1, 2, 3, 4::Int]
    sig1 <- signalFromList [11, 12, 13, 14]
    sig2 <- signalFromList [21, 22, 23, 24]
    sig3 <- signalFromList [31, 32, 33, 34]
    sigSig <- signalFromList [sig0, sig3, sig2, sig1]
    joinS sigSig
  r @?= [1, 32, 23, 14]

test_delayS = do
  r <- networkToListGC 4 $ do
    sig <- signalFromList [1, 2, 3, 4::Int]
    delayS (-1) sig
  r @?= [-1, 1, 2, 3]

test_generatorE = do
  r <- networkToListGC 4 $ do
    evSig <- generatorE =<< eventFromList [[subnet0], [subnet1], [subnet2], [subnet3]]
    let sigSig = head <$> eventToSignal evSig
    joinS sigSig
  r @?= [1, 11, 21, 31]
  where
    subnet0 = signalFromList [1, 2, 3, 4::Int]
    subnet1 = signalFromList [11, 12, 13, 14]
    subnet2 = signalFromList [21, 22, 23, 24]
    subnet3 = signalFromList [31, 32, 33, 34]

test_generatorE1 = do
  r <- networkToListGC 4 $ do
    evEv <- generatorE =<<
      eventFromList [[subnet 0], [subnet 1, subnet 2], [], [subnet 3]]
    dEv <- accumD mempty $ const <$> evEv
    ev <- joinDE dEv
    return $ eventToSignal ev
  r @?= [[1], [21], [22, 23], [31]]
  where
    subnet k = fmap (10*k+) <$> eventFromList [[1], [2,3], [], [4::Int]]

test_accumE = do
  r <- networkToList 3 $ do
    strS <- signalFromList ["foo", "", "baz"]
    accE <- scanE "<>" $ append <$> signalToEvent strS
    return $ eventToSignal accE
  r @?= [["<>f", "<>fo", "<>foo"], [], ["<>foob", "<>fooba", "<>foobaz"]]
  where
    append ch str = str ++ [ch]

test_fmapSignal = do
  succCountRef <- newRef (0::Int)
  r <- networkToListGC 3 $ do
    chS <- signalFromList ['f', 'a', 'r']
    let fchS = mysucc succCountRef <$> chS
    return $ comb <$> fchS <*> (mysucc succCountRef <$> fchS)
  r @?= ["gh", "bc", "st"]
  count <- readRef succCountRef
  count @?= 6
  where
    {-# NOINLINE mysucc #-}
    mysucc ref c = unsafePerformIO $ do
      modifyRef ref (+1)
      return $ succ c
    comb x y = [x, y]

test_applySE = do
  r <- networkToListGC 3 $ do
    evt <- eventFromList ["ab", "", "c"]
    sig <- signalFromList [0, 1, 2::Int]
    return $ eventToSignal $ (,) <$> sig <@> evt
  r @?= [[(0, 'a'), (0, 'b')], [], [(2, 'c')]]

test_joinDD = do
  r <- networkToList 5 net
  r1 <- networkToListGC 5 net
  r @?= ["0a", "1b", "1b", "1c", "0d"]
  r1 @?= r
  where
    net = do
      inner0 <- discrete "0a" [[], ["0b"], [], ["0c"], ["0d"]]
      inner1 <- discrete "1a" [[], ["1b"], [], ["1c"], ["1d"]]
      outer <- discrete inner0 [[], [inner1], [], [], [inner0]]
      discreteToSignal <$> joinDD outer

    discrete initial list = do
      evt <- eventFromList list
      accumD initial $ const <$> evt

test_joinDE = do
  r <- networkToList 5 net
  r1 <- networkToListGC 5 net
  r @?= [[], ["1b"], [], ["1c"], ["0d"]]
  r1 @?= r
  where
    net = do
      inner0 <- eventFromList [[], ["0b"], [], ["0c"], ["0d"]]
      inner1 <- eventFromList [[], ["1b"], [], ["1c"], ["1d"]]
      outer <- discrete inner0 [[], [inner1], [], [], [inner0]]
      eventToSignal <$> joinDE outer

    discrete initial list = do
      evt <- eventFromList list
      accumD initial $ const <$> evt

test_joinDS = do
  r <- networkToList 4 net
  r1 <- networkToListGC 4 net
  r @?= ["0a", "1b", "1c", "0d"]
  r1 @?= r
  where
    net = do
      inner0 <- signalFromList ["0a", "0b", "0c", "0d"]
      inner1 <- signalFromList ["1a", "1b", "1c", "1d"]
      outer <- discrete inner0 [[], [inner1], [], [inner0]]
      joinDS outer

    discrete initial list = do
      evt <- eventFromList list
      accumD initial $ const <$> evt

test_mfix = do
  r <- networkToList 3 net
  r @?= [1, 6, 30]
  where
    net = fmap snd $ mfix $ \ ~(e', _) -> do
      r <- accumD 1 $ (*) <$> e'
      e <- eventFromList [[], [2,3], [5::Int]]
      return (e, discreteToSignal r)

test_orderingViolation_joinDS = do
  g <- start net
  g >>= (@?=(0::Int))
  g >>= (@?=1)
  shouldThrowOrderingViolation g
  where
    net = fmap snd $ mfix $ \ ~(sd', _) -> do
      s <- joinDS sd'
      se <- eventFromList [[], [pure 1], [s]]
      sd <- accumD (pure 0) $ const <$> se
      return (sd, s)

test_externalEvent = do
  ref <- newRef []
  ee <- newExternalEvent
  triggerExternalEvent ee "foo"
  readRef ref >>= (@?=[])
  w <- mkWeakWithIORef ref (modifyRef ref . (:)) Nothing
  listenToExternalEvent ee (weakToLike w)
  triggerExternalEvent ee "bar"
  readRef ref >>= (@?=["bar"])
  triggerExternalEvent ee "baz"
  readRef ref >>= (@?=["baz", "bar"])

test_externalE = do
  ee <- newExternalEvent
  triggerExternalEvent ee "a"
  g <- start $ eventToSignal <$> externalE ee
  triggerExternalEvent ee "b"
  g >>= (@?=["b"])
  g >>= (@?=[])
  triggerExternalEvent ee "c"
  triggerExternalEvent ee "d"
  g >>= (@?=["c","d"])

test_takeWhileE = do
  finalizerRecord <- newRef []
  inputRefA <- newRef []
  inputRefB <- newRef []
  let add ident = modifyRef finalizerRecord (ident:)
  wA <- mkWeakWithIORef inputRefA inputRefA (Just $ add "A")
  wB <- mkWeakWithIORef inputRefB inputRefB (Just $ add "B")
  g <- start $ do
    sigA <- externalS $ readRef inputRefA
    sigB <- externalS $ readRef inputRefB
    evtA <- takeWhileE (>0) $ signalToEvent sigA
    evtB <- takeWhileE (>0) $ signalToEvent sigB
    return $ (,) <$> eventToSignal evtA <*> eventToSignal evtB

  performGC
  readRef finalizerRecord >>= (@?=[])

  writeToW wA [2, -1::Int]
  writeToW wB [1, 2::Int]
  g >>= (@?=([2], [1, 2]))
  performGC
  --readRef finalizerRecord >>= (@?=["A"])
  --- ^ this line doesn't work as expected, for some reason

  writeToW wA [3, 4]
  writeToW wB []
  g >>= (@?=([], []))
  performGC
  readRef finalizerRecord >>= (@?=["A"])

  writeToW wA [5, 6]
  writeToW wB [3]
  g >>= (@?=([], [3]))
  performGC
  readRef finalizerRecord >>= (@?=["A"])

  writeToW wA [7, 8]
  writeToW wB [-2]
  g >>= (@?=([], []))
  performGC
  readRef finalizerRecord >>= (@?=["B", "A"])
  where
    writeToW wRef val = do
      m'ref <- deRefWeak wRef
      case m'ref of
        Nothing -> return ()
        Just ref -> writeRef ref val

mkAccumCount n ac f = networkToList n $ do
  evt <- eventFromList $ map pure $ repeat 1
  eventToSignal <$> ac 0 ((\i s -> f (i + s, i + s :: Int)) <$> evt)

test_mapAccumE = do
  r <- mkAccumCount 10 mapAccumE id
  r @?= (take 10 $ map pure (iterate (+1) 1))

test_mapAccumEM = do
  r <- networkToList 16 $ do
    evt <- eventFromList $ map pure $ iterate (+1) 0
    eE <- mapAccumEM 0 ((\s n -> do e <- eventFromList (replicate n [] ++ [[n]]); return (n+s, e)) <$> evt)
    intE <- joinDE =<< accumD mempty (mappend <$> eE)
    return $ eventToSignal intE
  r @?= [[0],[0],[],[1],[],[],[3],[],[],[],[6],[],[],[],[],[10]]

test_mapAccumEquivalent = do
  r1 <- mkAccumCount 10 mapAccumE id
  r2 <- mkAccumCount 10 mapAccumEM return
  r1 @?= r2

test_delayE = do
  r <- networkToList 4 $ do
    evt <- eventFromList ["ab", "", "c", "d"]
    eventToSignal <$> delayE evt
  r @?= ["", "ab", "", "c"]

shouldThrowOrderingViolation :: IO a -> Assertion
shouldThrowOrderingViolation x = do
  r <- f <$> try x
  r @?= True
  where
    f (Left e)
      | Just (OrderingViolation _) <- fromException e
      = True
    f _ = False

_tests :: IO Counts
_tests = runTestTT tests

-- vim: sw=2 ts=2 sts=2
