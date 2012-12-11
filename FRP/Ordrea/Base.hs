{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoRec #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module FRP.Ordrea.Base
  ( SignalGen
  , Signal, Event, Discrete

  , ExternalEvent
  , newExternalEvent, triggerExternalEvent, listenToExternalEvent

  , generatorE, filterE, stepClockE, dropStepE, eventFromList
  , scanE, mapAccumE, mapAccumEM
  , accumE, scanAccumE, scanAccumEM
  , mapMaybeE, justE, flattenE, expandE, externalE
  , takeWhileE

  , joinDD, joinDE, joinDS

  , start, externalS, joinS, delayS, signalFromList, networkToList
  , networkToListGC

  , accumD, changesD, preservesD, delayD

  , eventToSignal, signalToEvent, applySE

  , discreteToSignal

  , TimeFunction(..), (<@>)
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Char as Char
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Typeable
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.IO.Unsafe
import System.Mem (performGC)
import System.Mem.Weak
import Test.HUnit

import FRP.Ordrea.Weak

-- Phases
--
-- Execution of an ordrea program can be broken down into the
-- following phases:
--
-- * Construction (SignalGen monad)
--   The construction phase is the first step to construct a new
--   (sub)network. In this phase, new nodes are created and a fresh location
--   is assigned to each dynamic node. This is the only phase the user
--   describes directly.
-- * Initialization (Initialize monad)
--   This phase completes construction of a new (sub)network. Nodes are
--   connected together in this phase.
-- * Execution step (Run monad)
--   This phase updates internal states of the network, moving the
--   computation one step forward.
-- * Clean-up step (Cleanup monad)
--   This phase comes after each execution step. The 'current state' of
--   an event node is reset to [] in this phase.
--
-- For the toplevel network the construction phase comes before the first
-- execution step. On the other hand, a subnetwork is constructed during
-- an execution step and is immediately updated as part of the current
-- execution step.
--
-- (Rationale)
-- + Why is a separate initialization phase needed?
-- - Because functions in the SignalGen monad cannot examine given signals.
--   Doing so would result in a NonTermination exception in case of a circular
--   circuit.

newtype SignalGen a = SignalGen (ReaderT GEnv IO a)
  deriving (Monad, Functor, Applicative, MonadIO, MonadFix)
type Initialize = ReaderT IEnv IO
type Run = ReaderT REnv IO
type Cleanup = IO

data Signal a   = Sig !Priority !(Initialize (Pull a))
data Event a    = Evt !Priority !(Initialize (Pull [a], Notifier))
data Discrete a = Dis !Priority !(Initialize (Pull a, Notifier))

type Consumer a = a -> IO ()

----------------------------------------------------------------------
-- callback modes

data CallbackMode = Pull | Push
  deriving (Eq, Show)

whenPush :: CallbackMode -> Run () -> Run ()
whenPush Push x = x
whenPush Pull _ = return ()

----------------------------------------------------------------------
-- locations and priorities

-- Location of a dynamic node.
type Location = U.Vector Word

-- Priority of updates.
data Priority = Priority
  { priLoc :: {-- UNPACK #-} !Location
  , priNum :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord) -- The default lexicographical ordering is appropriate

-- just for debugging
instance Show Priority where
  show Priority{priLoc = loc, priNum = num} =
    show (U.toList loc) ++ "/" ++ show num

data OrderingViolation = OrderingViolation String
  deriving (Show, Typeable)

instance Exception OrderingViolation

nextPrio :: Priority -> Priority
nextPrio prio@Priority{priNum=n} = prio{ priNum = n + 1 }

bottomLocation :: Location
bottomLocation = U.empty

bottomPrio :: Location -> Priority
bottomPrio loc = Priority
  { priLoc = loc
  , priNum = 0
  }

newLocationGen :: Location -> IO (IO Location)
newLocationGen parentLoc = do
  counter <- newRef 0
  return $ do
    num <- readRef counter
    writeRef counter $! num + 1
    return $! parentLoc `U.snoc` num

-- Check the ordering condition.
shouldBeGreaterThan :: Priority -> Priority -> Initialize ()
shouldBeGreaterThan x y = do
  debug $ "shouldBeGreaterThan: " ++ msg
  unless (x > y) $ liftIO $ throwIO $ OrderingViolation msg
  where
    msg = show (x, y)

----------------------------------------------------------------------
-- weak pointers

data WeakKey = forall a. WeakKey {-# UNPACK #-} !(IORef a)

mkWeakWithKey :: WeakKey -> v -> IO (Weak v)
mkWeakWithKey (WeakKey ref) v = mkWeakWithIORef ref v Nothing

newtype WeakLike a = WeakLike (IO (Maybe a))
  deriving (Functor)

weakToLike :: Weak a -> WeakLike a
weakToLike = WeakLike . deRefWeak

deRefWeakLike :: WeakLike a -> IO (Maybe a)
deRefWeakLike (WeakLike a) = a

----------------------------------------------------------------------
-- SignalGen monad

data GEnv = GEnv
  { envRegisterInit :: Consumer (Initialize ())
  , envRegisterPrep :: Consumer (Run ())
  , envGenLocation :: IO Location
  }

runSignalGen
  :: Consumer (Run ()) -> Location -> Notifier -> SignalGen a
  -> IO (a, Run ())
runSignalGen regPrep parentLoc clock (SignalGen gen) = do
  (registerI, runAccumI) <- newActionAccum
  locGen <- newLocationGen parentLoc
  let
    genv = GEnv
      { envRegisterInit = registerI
      , envRegisterPrep = regPrep
      , envGenLocation = locGen
      }
  result <- runReaderT gen genv
  (_, runAccumF) <- runInit parentLoc clock runAccumI
  return (result, runAccumF)

runSignalGenInStep :: SignalGen (Location -> Notifier -> SignalGen a -> Run a)
runSignalGenInStep = do
  regPrep <- getPreparationAdder
  return $ \parentLoc clock sgen -> debugFrame "SGenInStep" $ do
    (result, initial) <- liftIO $ runSignalGen regPrep parentLoc clock sgen
    isolatingUpdates initial
    return result

runSignalGenToplevel :: SignalGen (Initialize a) -> IO (a, Run ())
runSignalGenToplevel gen = do
  (clock, clockTrigger) <- newNotifier
  prepVar <- newEmptyMVar
  (val, initial) <- debugFrame "toplevel" $ do
    ref <- newRef undefined
    ((), initial) <- runSignalGen (addToPrep prepVar) bottomLocation clock $ do
      i <- gen
      registerInit $ writeRef ref =<< i
    val <- readRef ref
    return (val, initial)
  liftIO $ putMVar prepVar initial
  return (val, join $ liftIO $ swapMVar prepVar clockTrigger)
  where
    addToPrep prepVar x = modifyMVar_ prepVar (\r -> return (x >> r))

genLocation :: SignalGen Location
genLocation = SignalGen $ do
  gen <- asks envGenLocation
  lift gen

registerInit :: Initialize () -> SignalGen ()
registerInit ini = SignalGen $ do
  reg <- asks envRegisterInit
  frm <- debugGetFrame
  lift $ reg $ debugPutFrame "init" frm ini

getPreparationAdder :: SignalGen (Run () -> IO ())
getPreparationAdder = SignalGen $ asks envRegisterPrep

----------------------------------------------------------------------
-- Initialize monad

data IEnv = IEnv
  { envClock :: Notifier
  , envParentLocation :: Location
  , envRegisterFirstStep :: Consumer (Run ())
  }

registerFirstStep :: Run () -> Initialize ()
registerFirstStep action = do
  reg <- asks envRegisterFirstStep
  frm <- debugGetFrame
  lift $ reg $ debugPutFrame "fst" frm action

getClock :: Initialize Notifier
getClock = asks envClock

_getParentLocation :: Initialize Location
_getParentLocation = asks envParentLocation

runInit :: Location -> Notifier -> Initialize a -> IO (a, Run ())
runInit parentLoc clock i = do
  (registerF, runAccumF) <- newActionAccum
  let
    ienv = IEnv
      { envClock = clock
      , envRegisterFirstStep = registerF
      , envParentLocation = parentLoc
      }
  result <- runReaderT i ienv
  return (result, runAccumF)

-- | Creates a function that runs an @Initialize@ action inside @Run@,
-- using @loc@ as the parent location.
makeSubinitializer :: Location -> Initialize (Initialize a -> Run a)
makeSubinitializer loc = do
  clock <- getClock
  return $ \sub -> do
    (result, first) <- liftIO $ runInit loc clock sub
    isolatingUpdates $ debugFrame "makeSubInit.first" first
    return result

----------------------------------------------------------------------
-- Run monad

data REnv = REnv
  { envRegisterFini :: Consumer (Cleanup ())
  , envPendingUpdates :: IORef (M.Map Priority (Run ())) -- TODO: use heap?
  }

runRun :: Run a -> IO a
runRun run = debugFrame "runRun" $ do
  (registerF, runAccumF) <- liftIO newActionAccum
  pqueueRef <- newRef M.empty
  let
    renv = REnv
      { envRegisterFini = registerF
      , envPendingUpdates = pqueueRef
      }
  result <- runReaderT (run <* runUpdates) renv
  debugFrame "fini" runAccumF
  return result

runUpdates :: Run ()
runUpdates = debugFrame "runUpdates" $ asks envPendingUpdates >>= loop
  where
    loop pqueueRef = do
      pending <- readRef pqueueRef
      case M.minViewWithKey pending of
        Nothing -> return ()
        Just ((prio, upd), next) -> do
          debug $ "running updates with prio " ++ show prio
          writeRef pqueueRef next
          upd :: Run ()
          loop pqueueRef

registerFini :: IO () -> Run ()
registerFini fini = do
  reg <- asks envRegisterFini
  frm <- debugGetFrame
  lift $ reg $ debugPutFrame "fini" frm fini

registerUpd :: Priority -> Run () -> Run ()
registerUpd prio upd = do
  pqueueRef <- asks envPendingUpdates
  modifyRef pqueueRef $ M.insertWith' (>>) prio upd

isolatingUpdates :: Run a -> Run a
isolatingUpdates action = do
  pqueueRef <- asks envPendingUpdates
  pqueue <- readRef pqueueRef
  writeRef pqueueRef M.empty
  debug "isolating updates"
  result <- action
  debug "running isolated updates"
  runUpdates
  debug "done running isolated updates"
  writeRef pqueueRef pqueue
  return result

----------------------------------------------------------------------
-- push

type Notifier = NotifierG Run
type NotifierG m = WeakLike (m ()) -> IO ()

listenToNotifier :: WeakKey -> Notifier -> Run () -> Initialize ()
listenToNotifier key push handler = do
  frm <- debugGetFrame
  weak <- liftIO $ mkWeakWithKey key (debugPutFrame "notifier" frm handler)
  liftIO $ push (weakToLike weak)

listenToNotifierOnce :: (MonadIO m) => Notifier -> Run () -> m ()
listenToNotifierOnce push handler = do
  ref <- liftIO $ newIORef (0 :: Int)
  let h' = liftIO (modifyIORef ref (+1)) >> handler
  liftIO $ push $ WeakLike $ do
    n <- liftIO $ readIORef ref
    return $ if n > 0
      then Nothing
      else Just h'

newNotifier :: (Functor m, MonadIO m) => IO (NotifierG m, m ())
newNotifier = do
  listenersRef <- newRef []
  return (register listenersRef, invoke listenersRef)
  where
    register ref listenerWeak = modifyRef ref (listenerWeak:)

    invoke ref = do
      weaks <- readRef ref
      (weaks', listeners) <- unzip . catMaybes <$> mapM run1 weaks
      sequence_ $ reverse listeners
      writeRef ref weaks'
      where
        run1 weak = liftIO $ fmap ((,) weak) <$> deRefWeakLike weak

emptyNotifier :: Notifier
emptyNotifier _weak = return ()

----------------------------------------------------------------------
-- pull

type Pull a = Run a

newCachedPull :: Initialize (Run a) -> SignalGen (Pull a)
newCachedPull gencalc = do
  actionRef <- newRef (error "newCachedPull: not initialized")
  registerInit $ writeRef actionRef =<< primStepMemo =<< gencalc
  return $ join $ readRef actionRef

pullFromCache
  :: IORef (Maybe a)
  -> Run a
  -> Run ()
  -> Pull a
pullFromCache ref pull onWrite = do
  cache <- readRef ref
  case cache of
    Nothing -> do
      val <- pull
      writeRef ref (Just val)
      onWrite
      return val
    Just val -> return val

-- Caching and memoization
--
-- In this module, the terms 'caching' and 'memoization' refer to two different
-- things:
--
-- * Caching is a state manipulation to make sure that a node has only one
--   copy of its internal state, even if it's referenced from multiple places.
-- * Memoization is a state manipulation to avoid calculatig the same value
--   twice, even if it's repeatedly queried. For example,
--   in the expression (f <$> d) where d is a Discrete, it's important not to
--   call f multiple times when the value of d hasn't changed.
--
-- In general, omitting caching is safe if the node is referenced from only
-- one place, but ommitting memoization is only safe if it has just one consumer
-- AND that consumer only asks the current value when the value has been updated
-- since the last read.

-- | Memoize a @Pull@. The underlyng @Pull@ will be called at most once per step.
primStepMemo :: Pull a -> Initialize (Pull a)
primStepMemo pull = do
  memoRef <- newRef Nothing
  return $ pullFromCache memoRef pull $ registerFini $ writeRef memoRef Nothing

----------------------------------------------------------------------
-- common push-pull operations

unsafeProtectFromDup :: (a -> Initialize a) -> Initialize a -> Initialize a
unsafeProtectFromDup protect base = unsafeCache (base >>= protect)

-- | @unsafeCache a@ is an idempotent initialization action made from @a@.
-- When it's run for the first time, @a@ is executed. For subsequent executions
-- the return value from the first call will be returned without causing
-- any effects.
--
-- Note that this function is not referentially transparent.
unsafeCache :: Initialize a -> Initialize a
unsafeCache action = do
  cache <- readRef cacheRef
  case cache of
    Just val -> return val
    Nothing -> do
      val <- action
      writeRef cacheRef (Just val)
      return val
  where
    cacheRef = unsafeDupablePerformIO $ newIORef (const' Nothing action)
{-# NOINLINE unsafeCache #-}

-- | Non-inlinable version of const, only useful to prevent optimization.
const' :: a -> b -> a
const' x _ = x
{-# NOINLINE const' #-}

transparentMemoD
  :: Initialize (Pull a, Notifier)
  -> Initialize (Pull a, Notifier)
transparentMemoD orig = unsafeProtectFromDup primDiscreteMemo orig

transparentMemoE
  :: Initialize (Pull [a], Notifier)
  -> Initialize (Pull [a], Notifier)
transparentMemoE orig = unsafeProtectFromDup primEventMemo orig

transparentMemoS :: Initialize (Pull a) -> Initialize (Pull a)
transparentMemoS orig = unsafeProtectFromDup primStepMemo orig

primDiscreteMemo :: (Pull a, Notifier) -> Initialize (Pull a, Notifier)
primDiscreteMemo (pull, notifier) = do
  ref <- newRef Nothing
  listenToNotifier (WeakKey ref) notifier $
    writeRef ref . Just =<< pull
  return (pullFromCache ref pull (return ()), notifier)

primEventMemo :: (Pull [a], Notifier) -> Initialize (Pull [a], Notifier)
primEventMemo (pull, notifier) = do
  ref <- newRef Nothing
  listenToNotifier (WeakKey ref) notifier $
    writeRef ref . Just =<< pull
  return (pullFromCache ref pull (resetCache ref), notifier)
  where
    resetCache ref = registerFini $ writeRef ref (Just [])

listenToPullPush
  :: WeakKey
  -> Pull a
  -> Notifier
  -> Priority
  -> (CallbackMode -> a -> Run ())
  -> Initialize ()
listenToPullPush key pull notifier prio handler = do
  registerFirstStep $ registerUpd prio $ handler Pull =<< pull
    --- ^ use pull for the first step
  listenToNotifier key notifier $ handler Push =<< pull
    --- ^ use push for the subsequent steps

----------------------------------------------------------------------
-- external events

-- | Push-based asynchronous events.
newtype ExternalEvent a = ExternalEvent (MVar (NotifierG IO, IO (), IORef a))

eeVoid :: a
eeVoid = error "bug: ExternalEvent: void"

newExternalEvent :: IO (ExternalEvent a)
newExternalEvent = do
  (add, invoke) <- newNotifier
  ref <- newRef eeVoid
  ExternalEvent <$> newMVar (add, invoke, ref)

listenToExternalEvent :: ExternalEvent a -> WeakLike (a -> IO ()) -> IO ()
listenToExternalEvent (ExternalEvent var) handlerW =
  withMVar var $ \(add, _, ref) -> add $ invoke ref <$> handlerW
  where
    invoke ref handler = do
      val <- readRef ref
      handler val

triggerExternalEvent :: ExternalEvent a -> a -> IO ()
triggerExternalEvent (ExternalEvent var) val = withMVar var $ \(_, invoke, ref) -> do
  writeRef ref val
  invoke
  writeRef ref eeVoid

----------------------------------------------------------------------
-- events

instance Functor Event where
  fmap f = transformEvent1 (map f)

instance Monoid (Event a) where
  mempty = emptyEvent
  mappend x y = mergeEvents [x, y]
  mconcat = mergeEvents

listenToEvent
  :: WeakKey
  -> Event a
  -> Priority
  -> (CallbackMode -> [a] -> Run ())
  -> Initialize ()
listenToEvent key (Evt evtprio evt) prio handler = debugFrame "listenToEvent" $ do
  prio `shouldBeGreaterThan` evtprio
  (evtPull, evtNot) <- evt
  listenToPullPush key evtPull evtNot prio $ \mode occs ->
    when (not $ null occs) $ handler mode occs

newEventSG :: Priority -> SignalGen (Event a, CallbackMode -> [a] -> Run (), WeakKey)
newEventSG prio = do
  ref <- newRef []
  (push, trigger) <- liftIO newNotifier
  let evt = Evt prio $ return (eventPull ref, push)
  return (evt, eventTrigger ref trigger, WeakKey ref)

newEventInit :: Initialize ((Pull [a], Notifier), CallbackMode -> [a] -> Run (), WeakKey)
newEventInit = do
  ref <- newRef []
  (push, trigger) <- liftIO newNotifier
  return ((eventPull ref, push), eventTrigger ref trigger, WeakKey ref)

eventPull :: IORef [a] -> Pull [a]
eventPull buf = readRef buf

eventTrigger :: IORef [a] -> Run () -> CallbackMode -> [a] -> Run ()
eventTrigger buf notify mode occs = do
  writeRef buf occs
  registerFini $ do
    debug "clearing event ref"
    writeRef buf []
  whenPush mode notify

transformEvent :: ([a] -> [b]) -> Event a -> Event b
transformEvent f parent@(Evt evprio _) = Evt prio $ debugFrame "transformEvent" $ transparentMemoE $ do
  (pullpush, trigger, key) <- newEventInit
  listenToEvent key parent prio $ \mode xs -> case f xs of
    [] -> do
      debug $ "transformEvent: prio=" ++ show prio ++ " -> []"
      return ()
    ys -> do
      debug $ "transformEvent: prio=" ++ show prio ++ " -> len:" ++ show (length ys)
      trigger mode ys
  return pullpush
  where
    prio = nextPrio evprio

transformEvent1 :: ([a] -> [b]{-non-empty-}) -> Event a -> Event b
transformEvent1 f (Evt evprio evt) = Evt prio $ debugFrame "transformEvent1" $ transparentMemoE $ do
  (pull, notifier) <- evt
  return (f <$> pull, notifier)
  where
    prio = nextPrio evprio

generatorE :: Event (SignalGen a) -> SignalGen (Event a)
generatorE evt = do
  here <- genLocation
  let prio = bottomPrio here
  (result, trigger, key) <- newEventSG prio
  runSG <- runSignalGenInStep
  registerInit $ do
    clock <- getClock
    listenToEvent key evt prio $ \mode gens ->
      trigger mode =<< mapM (runSG here clock) gens
  return result

mergeEvents :: [Event a] -> Event a
mergeEvents [] = emptyEvent
mergeEvents evts = Evt prio $ unsafeCache $ do
  (pullpush, trigger, key) <- newEventInit
  occListRef <- newRef []
  let
    upd mode = do
      occList <- readRef occListRef
      debug $ "mergeEvents: upd: prio=" ++ show prio ++ "; total occs=" ++ show (length $ concatMap snd occList)
      when (not $ null occList) $ do
        writeRef occListRef []
        trigger mode $ concatMap snd $ sortBy (comparing fst) occList
  forM_ (zip [0::Int ..] evts) $ \(num, evt) ->
    listenToEvent key evt prio $ \mode occs -> do
      debug $ "mergeEvents: listen: noccs=" ++ show (length occs)
      modifyRef occListRef ((num, occs):)
      registerUpd prio $ upd mode
  return pullpush
  where
    prio = nextPrio $ maximum $ map evtPrio evts
    evtPrio (Evt p _) = p

emptyEvent :: Event a
emptyEvent = Evt (bottomPrio bottomLocation) $ return (return [], emptyNotifier)

filterE :: (a -> Bool) -> Event a -> Event a
filterE p = transformEvent (filter p)

stepClockE :: Event ()
stepClockE = Evt (bottomPrio bottomLocation) $ do
  clock <- getClock
  return (pure [()], clock)

dropStepE :: Event a -> SignalGen (Event a)
dropStepE ~(Evt evtprio evt) = do
  (result, trigger, key) <- newEventSG prio

  -- here we manually do the latter half of listenToPullPush
  registerInit $ do
    (getoccs, evtnotifier) <- evt
    listenToNotifier key evtnotifier $ do
      occs <- getoccs
      when (not $ null occs) $ trigger Push occs

  return result
  where
    prio = nextPrio evtprio

eventFromList :: [[a]] -> SignalGen (Event a)
eventFromList occs = signalToEvent <$> signalFromList (occs ++ repeat [])

scanE :: a -> Event (a -> a) -> SignalGen (Event a)
scanE initial evt@(~(Evt evtprio _)) = do
  (myevt, trigger, key) <- newEventSG prio
  ref <- newRef initial
  registerInit $ listenToEvent key evt prio $ \mode occs -> do
    debug $ "accumE: occs=" ++ show (length occs)
    oldVal <- readRef ref
    let _:vals = scanl (flip ($)) oldVal occs
    writeRef ref $ last vals
    trigger mode vals
  return myevt
  where
    prio = nextPrio evtprio

mapAccumE :: s -> Event (s -> (s, a)) -> SignalGen (Event a)
mapAccumE initial evt@(~(Evt evtprio _)) = do
  (myevt, trigger, key) <- newEventSG prio
  ref <- newRef initial
  registerInit $ listenToEvent key evt prio $ \mode occs -> do
    debug $ "mapAccumE: occs=" ++ show (length occs)
    oldVal <- readRef ref
    let (newVal, occs') = mapAccumL (flip ($)) oldVal occs
    writeRef ref $ newVal
    trigger mode occs'
  return myevt
  where
    prio = nextPrio evtprio

mapAccumEM :: s -> Event (s -> SignalGen (s, a)) -> SignalGen (Event a)
mapAccumEM initial evt = do
  rec
    e <- generatorE $ go <$> prevState <@> expandE evt
    state <- accumD initial (const . fst <$> e)
    prevState <- delayD initial state
  return . flattenE $ snd <$> e
  where
  go :: s -> [s -> SignalGen (s, a)] -> SignalGen (s, [a])
  go initial2 fs = do
    foldM (\(s, as) f -> do (s', a) <- f s; return (s', as ++ [a])) (initial2, []) fs

{-# DEPRECATED accumE "accumE has been renamed to scanE" #-}
accumE :: a -> Event (a -> a) -> SignalGen (Event a)
accumE = scanE

{-# DEPRECATED scanAccumE "scanAccumE has been renamed to mapAccumE" #-}
scanAccumE :: s -> Event (s -> (s, a)) -> SignalGen (Event a)
scanAccumE = mapAccumE

{-# DEPRECATED scanAccumEM "scanAccumEM has been renamed to mapAccumEM" #-}
scanAccumEM :: s -> Event (s -> SignalGen (s, a)) -> SignalGen (Event a)
scanAccumEM = mapAccumEM

mapMaybeE :: (a -> Maybe b) -> Event a -> Event b
mapMaybeE f = transformEvent (mapMaybe f)

justE :: Event (Maybe a) -> Event a
justE = transformEvent catMaybes

flattenE :: Event [a] -> Event a
flattenE = transformEvent concat

expandE :: Event a -> Event [a]
expandE = transformEvent1 (:[])

-- | Create a new event that occurs every time the given external event
-- occurs.
externalE :: ExternalEvent a -> SignalGen (Event a)
externalE ee = do
  occsVar <- liftIO $ newMVar []
  (evt, trigger, key) <- newEventSG (bottomPrio bottomLocation)
  addToPrep <- getPreparationAdder
  handler <- liftIO $ fmap weakToLike $
    mkWeakWithKey key $ add trigger addToPrep occsVar
  liftIO $ listenToExternalEvent ee handler
  return evt
  where
    add trigger addToPrep occsVar occ = do
      firstTime <- modifyMVar occsVar $ \occs -> return (occ:occs, null occs)
      when firstTime $ addToPrep $ do
        occs <- liftIO $ swapMVar occsVar []
        trigger Push $ reverse occs

takeWhileE :: (a -> Bool) -> Event a -> SignalGen (Event a)
takeWhileE cond ~(Evt evtprio evt) = do
  (push, trigger) <- liftIO $ newNotifier
  ref <- newRef $ error "takeWhileE"
  registerInit $ do
    (evtPull, evtNot) <- evt
    subref <- newRef evtPull
    writeRef ref ([], Just subref)
    listenToPullPush (WeakKey subref) evtPull evtNot prio $ \mode occs -> do
      (_, eventRef) <- readRef ref
      when (isJust eventRef) $ do
        let !(occs', rest) = span cond occs
        when (not $ null occs') $ do
          modifyRef ref $ \(_, y) -> (occs', y)
          whenPush mode trigger
          registerFini $ modifyRef ref $ \(_, y) -> ([], y)
        when (not $ null rest) $ registerFini $ writeRef ref ([], Nothing)
  return $ Evt prio $ return (fst <$> readRef ref, push)
  where
    prio = nextPrio evtprio

----------------------------------------------------------------------
-- discretes

instance Functor Discrete where
  fmap = mapDiscrete

instance Applicative Discrete where
  pure = pureDiscrete
  (<*>) = apDiscrete

newDiscreteInit
  :: a
  -> Initialize ((Pull a, Notifier), CallbackMode -> a -> Run (), WeakKey)
newDiscreteInit initial = do
  ref <- newRef initial
  (push, trigger) <- liftIO newNotifier
  return ((readRef ref, push), discreteTrigger ref trigger, WeakKey ref)

newDiscreteSG
  :: a
  -> Priority
  -> SignalGen (Discrete a, Run a, CallbackMode -> a -> Run (), WeakKey)
newDiscreteSG initial prio = do
  ref <- newRef initial
  (push, trigger) <- liftIO newNotifier
  let dis = Dis prio $ return (readRef ref, push)
  return (dis, readRef ref, discreteTrigger ref trigger, WeakKey ref)

discreteTrigger :: IORef a -> Run () -> CallbackMode -> a -> Run ()
discreteTrigger buf notify mode val = do
  writeRef buf val
  whenPush mode notify

mapDiscrete :: (a -> b) -> Discrete a -> Discrete b
mapDiscrete f (Dis dprio dis) = Dis prio $ debugFrame "mapDiscrete" $ transparentMemoD $ do
  (pull, notifier) <- dis
  return (f <$> pull, notifier)
  where
    prio = nextPrio dprio

pureDiscrete :: a -> Discrete a
pureDiscrete value = Dis (bottomPrio bottomLocation) $
  return (pure value, emptyNotifier)

apDiscrete :: Discrete (a -> b) -> Discrete a -> Discrete b
-- both arguments must have been memoized
apDiscrete (Dis fprio fun) (Dis aprio arg)
    = Dis prio $ debugFrame "apDiscrete" $ transparentMemoD $ do
  dirtyRef <- newRef False
  isPullRef <- newRef False
  (pullpush, set, key) <- newDiscreteInit (error "apDiscrete: uninitialized")
  (funPull, funNot) <- fun
  (argPull, argNot) <- arg
  let
    upd = do
      debug $ "apDiscrete.upd; prio=" ++ show prio
      dirty <- readRef dirtyRef
      when dirty $ do
        isPull <- readRef isPullRef
        writeRef dirtyRef False
        writeRef isPullRef False
        set (if isPull then Pull else Push) =<< funPull <*> argPull
  let handler mode _ = do
        debug $ "apDiscrete.handler: prio=" ++ show prio
        writeRef dirtyRef True
        modifyRef isPullRef (|| mode==Pull)
        registerUpd prio upd
  listenToPullPush key funPull funNot prio handler
  listenToPullPush key argPull argNot prio handler
  return pullpush
  where
    srcprio = max fprio aprio
    prio = nextPrio srcprio

listenToDiscrete
  :: WeakKey
  -> Discrete a
  -> Priority
  -> (CallbackMode -> a -> Run ())
  -> Initialize ()
listenToDiscrete key (Dis disprio dis) prio handler = do
  prio `shouldBeGreaterThan` disprio
  (disPull, disNot) <- dis
  listenToPullPush key disPull disNot prio handler

joinDD :: Discrete (Discrete a) -> SignalGen (Discrete a)
joinDD outer@ ~(Dis outerprio _) = do
  here <- genLocation
  let prio = bottomPrio here
  outerRef <- newRef $ error "joinDD: outerRef not initialized"
  (push, trigger) <- liftIO newNotifier
  registerInit $ do
    prio `shouldBeGreaterThan` outerprio
    runSubinit <- makeSubinitializer here
    listenToDiscrete (WeakKey outerRef) outer prio $ \outerMode inner -> do
      debug $ "joinDD: outer; mode=" ++ show outerMode
      innerRef <- newRef $ error "joinDD: innerRef not initialized"
      writeRef outerRef innerRef
      runSubinit $ do
        listenToDiscrete (WeakKey innerRef) inner prio $ \innerMode val -> do
          currentInnerRef <- readRef outerRef
          when (currentInnerRef == innerRef) $ do
            debug $ "joinDD: inner; mode=" ++ show innerMode
            writeRef innerRef val
            case (outerMode, innerMode) of
              (Pull, Pull) -> return ()
              _ -> trigger
  return $ Dis prio $ return (readRef outerRef >>= readRef, push)

joinDE :: Discrete (Event a) -> SignalGen (Event a)
joinDE outer@ ~(Dis outerprio _) = do
  here <- genLocation
  let prio = bottomPrio here
  outerRef <- newRef $ error "joinDE: outerRef not initialized"
  (push, trigger) <- liftIO newNotifier
  registerInit $ do
    prio `shouldBeGreaterThan` outerprio
    runSubinit <- makeSubinitializer here
    listenToDiscrete (WeakKey outerRef) outer prio $ \outerMode inner -> do
      debug $ "joinDE: outer; mode=" ++ show outerMode
      innerRef <- newRef []
      writeRef outerRef innerRef
      runSubinit $ do
        listenToEvent (WeakKey innerRef) inner prio $ \innerMode occs -> do
          currentInnerRef <- readRef outerRef
          when (currentInnerRef == innerRef) $ do
            debug $ "joinDE: inner; mode=" ++ show innerMode
              ++ "; noccs=" ++ show (length occs)
            writeRef innerRef occs
            registerFini $ writeRef innerRef []
            case (outerMode, innerMode) of
              (Pull, Pull) -> return ()
              _ -> trigger
  return $ Evt prio $ return (readRef outerRef >>= readRef, push)

joinDS :: Discrete (Signal a) -> SignalGen (Signal a)
joinDS outer@ ~(Dis outerprio _) = do
  here <- genLocation
  let prio = bottomPrio here
  outerRef <- newRef $ error "joinDS: outerRef not initialized"
  registerInit $ do
    prio `shouldBeGreaterThan` outerprio
    runSubinit <- makeSubinitializer here
    listenToDiscrete (WeakKey outerRef) outer prio
        $ \_ (Sig innerprio sig) -> do
      debug $ "joinDS: outer"
      pull <- runSubinit $ do
        prio `shouldBeGreaterThan` innerprio
        sig
      writeRef outerRef pull
  return $ Sig prio $ return (readRef outerRef >>= id)

----------------------------------------------------------------------
-- signals

instance Functor Signal where
  fmap f (Sig prio pull) = Sig prio $ transparentMemoS $ fmap f <$> pull

instance Applicative Signal where
  pure x = Sig (bottomPrio bottomLocation) (return $ pure x)
  Sig f_prio f_init <*> Sig a_prio a_init =
      Sig (max f_prio a_prio) $ transparentMemoS $ do
    f_pull <- f_init
    a_pull <- a_init
    return $ f_pull <*> a_pull

start :: SignalGen (Signal a) -> IO (IO a)
start gensig = do
  (getval, prep) <- runSignalGenToplevel $ do
    Sig _ sig <- gensig
    return $ sig
  return $ runRun $ debugFrame "step" $ do
    debug "step"
    isolatingUpdates $ prep
    debugFrame "getval" getval

externalS :: IO a -> SignalGen (Signal a)
externalS get = do
  pull <- newCachedPull $ return $ liftIO get
  return $ Sig (bottomPrio bottomLocation) $ return pull

joinS :: Signal (Signal a) -> SignalGen (Signal a)
joinS ~(Sig _sigsigprio sigsig) = do
  here <- genLocation
  let prio = bottomPrio here
  pull <- newCachedPull $ do
    debug $ "joinS: making pull; prio=" ++ show prio
    runSubinit <- makeSubinitializer here
    sigpull <- sigsig
    return $ debugFrame ("joinS.pull[prio=" ++ show prio ++ "]") $ do
      Sig _sigprio sig <- sigpull
      pull <- runSubinit sig
      debugFrame "pull" pull
  return $! Sig prio $ return pull

delayS :: a -> Signal a -> SignalGen (Signal a)
delayS initial ~(Sig _sigprio sig) = do
  ref <- newRef initial
  registerInit $ do
    clock <- getClock
    pull <- sig
    listenToNotifier (WeakKey ref) clock $ do
      newVal <- pull
      registerFini $ writeRef ref newVal
  return $ Sig prio $ return $ readRef ref
  where
    prio = bottomPrio bottomLocation

signalFromList :: [a] -> SignalGen (Signal a)
signalFromList xs = debugFrame "signalFromList" $ do
  clock <- dropStepE stepClockE
  suffixD <- accumD xs $ drop 1 <$ clock
  return $ discreteToSignal $ hd <$> suffixD
  where
    hd = fromMaybe (error "listToSignal: list exhausted") .
      listToMaybe

networkToList :: Int -> SignalGen (Signal a) -> IO [a]
networkToList count network = do
  smp <- start network
  replicateM count smp

networkToListGC :: Int -> SignalGen (Signal a) -> IO [a]
networkToListGC count network = do
  smp <- start network
  replicateM count (performGC >> smp)

----------------------------------------------------------------------
-- events and discretes

accumD :: a -> Event (a -> a) -> SignalGen (Discrete a)
accumD initial evt@(~(Evt evtprio _)) = do
  debug $ "accumD: creating; prio=" ++ show prio
  (dis, get, set, key) <- newDiscreteSG initial prio
  registerInit $ listenToEvent key evt prio $ \mode occs -> do
    debug $ "accumD: prio=" ++ show prio ++ "; occs=" ++ show (length occs)
    oldVal <- get
    set mode $! foldl' (flip ($)) oldVal occs
  return dis
  where
    prio = nextPrio evtprio

changesD :: Discrete a -> Event a
changesD (Dis disprio dis) = Evt prio $ unsafeCache $ do
  (pullpush, trigger, key) <- newEventInit
  (dispull, notifier) <- dis
  listenToNotifier key notifier $ trigger Push . (:[]) =<< dispull
  return pullpush
  where
    prio = nextPrio disprio

preservesD :: Discrete a -> SignalGen (Event a)
preservesD dis@ ~(Dis disprio _) = do
  (evt, trigger, key) <- newEventSG prio
  registerInit $ listenToDiscrete key dis prio $ \mode val -> trigger mode [val]
  return evt
  where
    prio = nextPrio disprio

delayD :: a -> Discrete a -> SignalGen (Discrete a)
delayD initial dis@ ~(Dis disprio _dis) = do
  (dis2, _get, set, key) <- newDiscreteSG initial (bottomPrio bottomLocation)
  registerInit $ do
    clock <- getClock
    listenToDiscrete key dis (nextPrio disprio) $ \_mode val ->
      listenToNotifierOnce clock $ set Push val
  return dis2

----------------------------------------------------------------------
-- events and signals

eventToSignal :: Event a -> Signal [a]
eventToSignal (Evt prio evt) = Sig prio $ do
  (pull, _push) <- evt
  return pull

signalToEvent :: Signal [a] -> Event a
signalToEvent (Sig sigprio sig) = Evt prio $ unsafeCache $ do
  debug "signalToEvent"
  sigpull <- sig
  (pullpush, trigger, key) <- newEventInit
    --- ^ Here we create a fresh event, even though its pull component
    -- will be identical to sigpull. This is because we want a new key
    -- to keep the new notifier alive as long as the new pull, rather
    -- than the original pull, is alive.
  clock <- getClock
  registerFirstStep $ registerUpd prio $ onclock sigpull (trigger Pull)
  listenToNotifier key clock $ registerUpd prio $ onclock sigpull (trigger Push)
  return pullpush
  where
    prio = nextPrio sigprio
    onclock sigpull trigger = do
      occs <- sigpull
      debug $ "signalToEvent: onclock prio=" ++ show prio
        ++ "; noccs=" ++ show (length occs)
      when (not $ null occs) $ trigger occs

applySE :: Signal (a -> b) -> Event a -> Event  b
applySE (Sig fprio fun) arg@(Evt aprio _)
    = Evt prio $ debugFrame "applySE" $ transparentMemoE $ do
  (pullpush, trigger, key) <- newEventInit
  funPull <- fun
  let
    upd mode occs = do
      debug $ "applySE; prio=" ++ show prio
      funVal <- funPull
      trigger mode $ map funVal occs
  listenToEvent key arg prio $ \mode occs -> do
    debug $ "applySE: prio=" ++ show prio
    registerUpd prio $ upd mode occs
  return pullpush
  where
    srcprio = max fprio aprio
    prio = nextPrio srcprio

----------------------------------------------------------------------
-- discretes and signals

discreteToSignal :: Discrete a -> Signal a
discreteToSignal (Dis prio dis) = Sig prio $ fst <$> dis

----------------------------------------------------------------------
-- classes

class TimeFunction s where
  toSignal :: s a -> Signal a

instance TimeFunction Signal where
  toSignal = id

instance TimeFunction Discrete where
  toSignal = discreteToSignal

infixl 4 <@> -- same as <$> and <*>

(<@>) :: (TimeFunction s) => s (a -> b) -> Event a -> Event b
f <@> a = applySE (toSignal f) a

----------------------------------------------------------------------
-- utils

newRef :: (MonadIO m) => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: (MonadIO m) => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: (MonadIO m) => IORef a -> a -> m ()
writeRef x v = liftIO $ writeIORef x v

modifyRef :: (MonadIO m) => IORef a -> (a -> a) -> m ()
modifyRef x f = do
  old <- readRef x
  writeRef x $! f old

-- TODO: specialize
newActionAccum :: (MonadIO m) => IO (Consumer (m ()), m ())
newActionAccum = do
  actions <- newRef []
  return (add actions, run actions)
  where
    add ref act = modifyIORef ref (act:)
    run ref = readRef ref >>= sequence_

----------------------------------------------------------------------
-- internal debugging

debug :: (MonadIO m) => String -> m ()
debug str = when debugTraceEnabled $ liftIO $ do
  stack <- readRef debugStackRef
  debugPrintWith (length stack) ('-':str)

debugStackRef :: IORef [String]
debugStackRef = unsafePerformIO $ newRef []
{-# NOINLINE debugStackRef #-}

debugPrintWith :: (MonadIO m) => Int -> String -> m ()
debugPrintWith level msg = liftIO $ putStrLn $ replicate level ' ' ++ msg

debugFrame :: (MonadIO m) => String -> m a -> m a
debugFrame loc body = if not debugTraceEnabled then body else do
  oldStack <- readRef debugStackRef
  debugPrintWith (length oldStack) loc
  writeRef debugStackRef (loc:oldStack)
  val <- body
  writeRef debugStackRef oldStack
  return val

debugGetFrame :: (MonadIO m) => m DebugFrame
debugGetFrame = DF `liftM` readRef debugStackRef

debugPutFrame :: (MonadIO m) => String -> DebugFrame -> m a -> m a
debugPutFrame loc (DF frame) = debugFrame $
  loc ++ "(" ++ intercalate "," frame ++ ")"

newtype DebugFrame = DF [String]

debugTraceEnabled :: Bool
debugTraceEnabled = False

----------------------------------------------------------------------
-- tests

_unitTest :: IO Counts
_unitTest = runTestTT $ test
  [ test_signalFromList
  , test_accumD
  , test_changesD
  , test_delayD
  , test_mappendEvent
  , test_fmapEvent
  , test_filterE
  , test_dropStepE
  , test_apDiscrete
  , test_apDiscrete1
  , test_eventFromList
  , test_preservesD
  , test_joinS
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
  , test_takeWhileE
  , test_mapAccumE
  , test_mapAccumEM
  , test_mapAccumEquivalent
  ]

test_signalFromList = do
  r <- networkToList 4 $ signalFromList ["foo", "bar", "baz", "quux"]
  r @?= ["foo", "bar", "baz", "quux"]

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
  r @?= [[], [], ["<>/'f'/'o'/'o'/'b'/'a'/'z'"]]
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
    return $ eventToSignal $
      changesD accD `mappend` (signalToEvent $ (:[]) <$> strS)
  r @?= [["foo"], [""], ["<>/'f'/'o'/'o'/'b'/'a'/'z'", "baz"]]
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

test_apDiscrete = do
  r <- networkToListGC 4 $ do
    ev0 <- signalToEvent <$> signalFromList [[], [], [1::Int], [2,3]]
    ev1 <- signalToEvent <$> signalFromList [[], [4], [], [5]]
    dis0 <- accumD 0 $ max <$> ev0
    dis1 <- accumD 0 $ max <$> ev1
    let dis = (*) <$> dis0 <*> dis1
    return $ eventToSignal $ changesD dis
  r @?= [[], [0], [4], [15]]

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
    accE <- accumE "<>" $ append <$> signalToEvent strS
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
  w <- mkWeakWithIORef ref (modifyIORef ref . (:)) Nothing
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

shouldThrowOrderingViolation :: IO a -> Assertion
shouldThrowOrderingViolation x = do
  r <- f <$> try x
  r @?= True
  where
    f (Left e)
      | Just (OrderingViolation _) <- fromException e
      = True
    f _ = False

-- vim: sw=2 ts=2 sts=2
