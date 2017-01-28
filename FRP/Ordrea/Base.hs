{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PatternGuards #-}

{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module FRP.Ordrea.Base
  ( SignalGen
  , Behavior, Event, Discrete

  , ExternalEvent
  , newExternalEvent, triggerExternalEvent, listenToExternalEvent

  , generatorE, filterE, stepClockE, dropStepE, eventFromList
  , scanE, mapAccumE, mapAccumEM
  , accumE, scanAccumE, scanAccumEM
  , mapMaybeE, justE, flattenE, expandE, externalE
  , takeWhileE, delayE

  , joinDD, joinDE, joinDB

  , start, externalB, joinB, delayB, behaviorFromList, networkToList
  , networkToListGC, snapshotB

  , scanD, changesD, preservesD, delayD

  , eventToBehavior, behaviorToEvent, applyBE

  , discreteToBehavior

  , TimeFunction(..), (<@>), (<@)

  , OrderingViolation (..)
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

import FRP.Ordrea.Weak
import UnitTest

-- Phases
--
-- Execution of an ordrea program can be broken down into the
-- following phases:
--
-- * Construction (SignalGen monad)
--   The construction phase is the first step to construct a new
--   (sub)network. In this phase, a fresh location is assigned to each dynamic
--   node to be constructed. 'delay' nodes are created in this phase.
--   This is the only phase the user describes directly.
-- * Initialization (Initialize monad)
--   This phase completes construction of a new (sub)network. Non-delay nodes
--   are created and all nodes get connected together. Initialization happens
--   when a SignalGen run for a (sub)network is completed, or when 'snapshot'
--   is called in a SignalGen run.
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

-- Behavior, Event and Discrete are represented as a pair of a priority (see Note
-- [Priority]) and an initialization action that returns the `core' of the
-- node. The initialization action is idempotent.

data Behavior a   = Beh !Priority !(Initialize (Pull a))
  --- ^ The pull contains the current value.
data Event a    = Evt !Priority !(Initialize (Pull [a], Push))
  --- ^ The pull contains the list of the current occurrences.
  -- The Push is active iff the list is non-empty.
data Discrete a = Dis !Priority !(Initialize (Pull a, Push))
  --- ^ The pull contains the current value.
  -- The Push is active iff the value might have changed.

type Consumer a = a -> IO ()

----------------------------------------------------------------------
-- locations and priorities

-- Note [Priority]
-- ~~~~~~~~~~~~~~~
--
-- Each node in a network has a "Priority". A Priority tells when in an
-- execution step the node will be updated. A smaller Priority means the node
-- gets updated earlier. To be precise:
--
-- An execution step is divided into substeps, one for each priority in the
-- network. An execution step begins with a substep for the minimum priority
-- (bottomPrio bottomLocation) and ends with a substep for the maximum priority
-- in the network. The following rules apply.
--
-- * A 'Pull' for a node with priority p will be ready after the substep for p
--   is complete or after the accompanying Push is triggered. The user
--   should not try to use the value before that.
-- * A 'Notifier' for a node with priority p, if it's active, is triggered
--   before the substep for p is complete. If it hasn't been triggered after
--   the substep, the user can be sure that it's inactive in the current step.

-- Location of a dynamic node. Each dynamic node gets a Location when it is
-- created. A Location is not necessarily unique, i.e. two dynamic nodes may
-- have the same Location.
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

-- | The next smallest priority after the given one.
nextPrio :: Priority -> Priority
nextPrio prio@Priority{priNum=n} = prio{ priNum = n + 1 }

-- | A special location which is never assigned to a dynamic node. Nodes that
-- don't depend on any dynamic node use this location.
bottomLocation :: Location
bottomLocation = U.empty

-- | The minimum priority under the given location.
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

-- | A Type-erasing wrapper for IORef. Its sole purpose is to serve as a key
-- of a weak pointer.
data WeakKey = forall a. WeakKey {-# UNPACK #-} !(IORef a)

-- | Create a weak pointer using an IORef wrapped inside WeakKey.
mkWeakWithKey :: WeakKey -> v -> IO (Weak v)
mkWeakWithKey (WeakKey ref) v = mkWeakWithIORef ref v Nothing

-- | Anything that can behave like a weak pointer.
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
  , envGenLocation :: IO Location
  , envIEnv :: IEnv
  }

-- Note [Global preparation accumulator]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- You can add actions to the global preparation accumulator, and they will be
-- executed at the beginning of the next step. Added actions will be
-- executed only once. It is global in the sense that subetworks see the same
-- accumulator as its parent does.
--
-- Actions can be added anytime anywhere, even in different threads.

-- | Run SignalGen in IO.
runSignalGen
  :: Consumer (Run ()) -- ^ see Note [Global preparation accumulator]
  -> Location -- ^ the location of the parent node of this subnetwork
  -> Push -- ^ the global clock notifier
  -> REnv -- ^ the Run env if we are in an execution step
  -> SignalGen a -- ^ an action to create a (sub)network
  -> IO a
runSignalGen regPrep parentLoc clock curStep (SignalGen gen) = do
  (registerI, runAccumI) <- newActionAccum
  locGen <- newLocationGen parentLoc
  let
    ienv = IEnv
      { envClock = clock
      , envParentLocation = parentLoc
      , envRegisterPrep = regPrep
      , envCurrentStep = curStep
      }
    genv = GEnv
      { envRegisterInit = registerI
      , envGenLocation = locGen
      , envIEnv = ienv
      }
  result <- runReaderT gen genv
  runInit ienv runAccumI
  return result

-- | Run SignalGen in the Run monad, as part of an execution step.
runSignalGenInStep :: SignalGen (Location -> Push -> SignalGen a -> Run a)
runSignalGenInStep = do
  regPrep <- getPreparationAdder
  return $ \parentLoc clock sgen -> debugFrame "SGenInStep" $ do
    renv <- ask
    liftIO $ runSignalGen regPrep parentLoc clock renv sgen

-- | Run a SignalGen action that constructs an independent network.
-- Returns a pair of the result and an "preparation" action to be performed
-- before each step.
runSignalGenToplevel :: SignalGen (Initialize a) -> Run (a, Run ())
runSignalGenToplevel gen = do
  renv <- ask
  (clock, clockTrigger) <- liftIO newPush
  prepClock clockTrigger
  liftIO $ do
    prepVar <- newMVar (prepClock clockTrigger)
    val <- debugFrame "toplevel" $ do
      ref <- newRef undefined
      runSignalGen (addToPrep prepVar) bottomLocation clock renv $ do
        i <- gen
        registerInit $ writeRef ref =<< i
      readRef ref
    return (val, join $ liftIO $ swapMVar prepVar (prepClock clockTrigger))
  where
    addToPrep prepVar x = modifyMVar_ prepVar (\r -> return (x >> r))
    prepClock clockTrigger = registerUpd (bottomPrio bottomLocation) clockTrigger

-- | Generate a new location.
genLocation :: SignalGen Location
genLocation = SignalGen $ do
  gen <- asks envGenLocation
  lift gen

-- | Register an initialization action to be performed after this SignalGen
-- is run.
registerInit :: Initialize () -> SignalGen ()
registerInit ini = SignalGen $ do
  reg <- asks envRegisterInit
  frm <- debugGetFrame
  lift $ reg $ debugPutFrame "init" frm ini

-- | Get access to the global preparation accumulator. See Note [Global
-- preparation accumulator].
getPreparationAdder :: SignalGen (Run () -> IO ())
getPreparationAdder = SignalGen $ asks $ envRegisterPrep . envIEnv

-- | Get the initialization environment to be used with this subnetwork.
getIEnv :: SignalGen IEnv
getIEnv = SignalGen $ asks envIEnv

----------------------------------------------------------------------
-- Initialize monad

data IEnv = IEnv
  { envClock :: Push
  , envParentLocation :: Location
  , envRegisterPrep :: Consumer (Run ())
  , envCurrentStep :: REnv
  }

-- | Get the global clock.
getClock :: Initialize Push
getClock = asks envClock

_getParentLocation :: Initialize Location
_getParentLocation = asks envParentLocation

-- | Run Initialize
runInit :: IEnv -> Initialize a -> IO a
runInit ienv i = runReaderT i ienv

-- | Creates a function that runs an @Initialize@ action inside @Run@,
-- using @loc@ as the parent location.
makeSubinitializer :: Location -> Initialize (Initialize a -> Run a)
makeSubinitializer loc = do
  parentIenv <- ask
  return $ \sub -> do
    renv <- ask
    liftIO $ runInit parentIenv { envCurrentStep = renv } sub

runInCurrentStep
  :: Run a -- ^ action to perform
  -> Initialize a
runInCurrentStep run = do
  renv <- asks envCurrentStep
  liftIO $ runReaderT run renv

registerNextStep :: Run () -> Initialize ()
registerNextStep x = do
  addPrep <- asks envRegisterPrep
  liftIO $ addPrep x

getPreparationAdderI :: Initialize (Run () -> IO ())
getPreparationAdderI = asks envRegisterPrep

----------------------------------------------------------------------
-- Run monad

data REnv = REnv
  { envRegisterFini :: Consumer (Cleanup ())
  , envPendingUpdates :: IORef (M.Map Priority (Run ())) -- TODO: use heap?
  }

-- | Run Run.
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
          debug $ "running substep for prio " ++ show prio
          writeRef pqueueRef next
          upd :: Run ()
          loop pqueueRef

-- | Register an action to be executed during the next clean-up step.
registerFini :: IO () -> Run ()
registerFini fini = do
  reg <- asks envRegisterFini
  frm <- debugGetFrame
  lift $ reg $ debugPutFrame "fini" frm fini

-- | Register an action to be executed in the substep for the specified
-- priority. See Note [Priority].
registerUpd :: Priority -> Run () -> Run ()
registerUpd prio upd = do
  pqueueRef <- asks envPendingUpdates
  modifyRef pqueueRef $ M.insertWith' (>>) prio upd

----------------------------------------------------------------------
-- push

-- | Push is a time-dependent boolean in a network. It is either True (active)
-- or False (inactive) in a given step. If you register a listener function
-- beforehand, it will be invoked once if the Push is active in the step.
-- If your function is not invoked during the time window it would be (see
-- Note [Priority]), you can know that the Push is inactive in this step.
-- Thus Push is capable of communicating 'False' in O(0) time, which is
-- the key to the asymptotic efficiency of the library.
--
-- The IORef contains whether this push has been triggered this generation.
data Push = Push !(NotifierG Run) {-# UNPACk #-} !(IORef Bool)

newPush :: IO (Push, Run ())
newPush = do
  (notifier, triggerPush) <- newNotifier
  activeRef <- newRef False
  let
    trigger = do
      writeRef activeRef True
      triggerPush
      registerFini $ writeRef activeRef False
  return (Push notifier activeRef, trigger)

-- | Register a callback to be called when the push is triggered.
-- It will be unregistered when the given WeakKey is invalidated.
listenToPush :: (MonadIO m) => WeakKey -> Push -> Run () -> m ()
listenToPush key (Push register _) handler = do
  frm <- debugGetFrame
  weak <- liftIO $ mkWeakWithKey key (debugPutFrame "notifier" frm handler)
  liftIO $ register (weakToLike weak)

-- | Register a one-time callback to be called when the notifier
-- is triggered. It will be unregistered after one invocation.
listenToPushOnce :: (MonadIO m) => Push -> Run () -> m ()
listenToPushOnce (Push register _) handler = do
  ref <- liftIO $ newIORef (0 :: Int)
  let h' = liftIO (modifyIORef ref (+1)) >> handler
  liftIO $ register $ WeakLike $ do
    n <- liftIO $ readIORef ref
    return $ if n > 0
      then Nothing
      else Just h'

-- | A push that is always inactive.
emptyPush :: Push
emptyPush = Push emptyNotifier emptyPushRef

emptyPushRef :: IORef Bool
emptyPushRef = unsafePerformIO $ newRef False
  -- noone should write to this
{-# NOINLINE emptyPushRef #-}

pushHasBeenTriggered :: Push -> Run Bool
pushHasBeenTriggered (Push _ ref) = readRef ref

-- | @NotifierG m@ lets you know when a particular type of event happens,
-- if you registere a callback, which is an action in the Monad @m@.
type NotifierG m = WeakLike (m ()) -> IO ()

-- | Create a new notifier. It returns a pair of the notifier and
-- a function to trigger it.
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

emptyNotifier :: NotifierG m
emptyNotifier _ = return ()

----------------------------------------------------------------------
-- pull

-- | A Pull reads the current value of a node. It must be idempotent
-- within a step. That is, calling it twice in a single step should
-- result in the same value, without much repeated overhead. A Pull
-- should not be called when it's not ready; See Note [Priority] for
-- details.
type Pull a = Run a

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
unsafeCache action = cacheWith cacheRef action
  where
    cacheRef = unsafeDupablePerformIO $ newIORef (const' Nothing action)
    {-# NOINLINE cacheRef #-}
{-# NOINLINE unsafeCache #-}

-- | Non-inlinable version of const, only useful to prevent optimization.
const' :: a -> b -> a
const' x _ = x
{-# NOINLINE const' #-}

-- | Return an idempotent version of the given initialization action using
-- the given IORef as a cache.
cacheWith :: IORef (Maybe a) -> Initialize a -> Initialize a
cacheWith cacheRef action = do
  cache <- readRef cacheRef
  case cache of
    Just val -> return val
    Nothing -> do
      val <- action
      writeRef cacheRef (Just val)
      return val

transparentMemoD
  :: Initialize (Pull a, Push)
  -> Initialize (Pull a, Push)
transparentMemoD orig = unsafeProtectFromDup primDiscreteMemo orig

transparentMemoE
  :: Initialize (Pull [a], Push)
  -> Initialize (Pull [a], Push)
transparentMemoE orig = unsafeProtectFromDup primEventMemo orig

transparentMemoS :: Initialize (Pull a) -> Initialize (Pull a)
transparentMemoS orig = unsafeProtectFromDup primStepMemo orig

primDiscreteMemo :: (Pull a, Push) -> Initialize (Pull a, Push)
primDiscreteMemo (pull, notifier) = do
  ref <- newRef Nothing
  listenToPush (WeakKey ref) notifier $
    writeRef ref . Just =<< pull
  return (pullFromCache ref pull (return ()), notifier)

primEventMemo :: (Pull [a], Push) -> Initialize (Pull [a], Push)
primEventMemo (pull, notifier) = do
  pull' <- primStepMemo pull
  return (pull', notifier)

listenToPullPush
  :: WeakKey
  -> Pull a
  -> Push
  -> Priority
  -> (a -> Run ())
  -> Initialize ()
listenToPullPush key pull notifier prio handler = do
  addPrep <- getPreparationAdderI
  runInCurrentStep $ registerUpd prio $ do
    handler =<< pull
    liftIO $ addPrep $
      listenToPush key notifier $ handler =<< pull

-- | Create a new node in the OrderGen monad, using the given initialization
-- action. The initialization action is probably time dependent, otherwise
-- you can make it a pure function rather than a SignalGen function.
-- The given initialization action is guaranteed to run exactly once,
-- before this construction step ends.
newNode
  :: Initialize a
  -> SignalGen (Initialize a)
newNode action = do
  ref <- newRef Nothing
  let act' = cacheWith ref action
  registerInit $ act' >> return ()
  return act'

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
  -> ([a] -> Run ())
  -> Initialize ()
listenToEvent key (Evt evtprio evt) prio handler = debugFrame "listenToEvent" $ do
  prio `shouldBeGreaterThan` evtprio
  (evtPull, evtNot) <- evt
  listenToPullPush key evtPull evtNot prio $ \occs ->
    when (not $ null occs) $ handler occs

newEventSG :: Priority -> SignalGen (Event a, [a] -> Run (), WeakKey)
newEventSG prio = do
  ref <- newRef []
  (push, trigger) <- liftIO newPush
  let evt = Evt prio $ return (eventPull ref, push)
  return (evt, eventTrigger ref trigger, WeakKey ref)

newEventInit :: Initialize ((Pull [a], Push), [a] -> Run (), WeakKey)
newEventInit = do
  ref <- newRef []
  (push, trigger) <- liftIO newPush
  return ((eventPull ref, push), eventTrigger ref trigger, WeakKey ref)

eventPull :: IORef [a] -> Pull [a]
eventPull buf = readRef buf

eventTrigger :: IORef [a] -> Run () -> [a] -> Run ()
eventTrigger buf notify occs = do
  writeRef buf occs
  registerFini $ do
    debug "clearing event ref"
    writeRef buf []
  notify

transformEvent :: ([a] -> [b]) -> Event a -> Event b
transformEvent f parent@(Evt evprio _) = Evt prio $ debugFrame "transformEvent" $ unsafeCache $ do
  (pullpush, trigger, key) <- newEventInit
  listenToEvent key parent prio $ \xs -> case f xs of
    [] -> do
      debug $ "transformEvent: prio=" ++ show prio ++ " -> []"
      return ()
    ys -> do
      debug $ "transformEvent: prio=" ++ show prio ++ " -> len:" ++ show (length ys)
      trigger ys
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
  runSG <- runSignalGenInStep
  fmap (Evt prio) $ newNode $ do
    (pullpush, trigger, key) <- newEventInit
    clock <- getClock
    listenToEvent key evt prio $ \gens ->
      trigger =<< mapM (runSG here clock) gens
    return pullpush

mergeEvents :: [Event a] -> Event a
mergeEvents [] = emptyEvent
mergeEvents evts = Evt prio $ unsafeCache $ do
  (pullpush, trigger, key) <- newEventInit
  occListRef <- newRef []
  let
    upd = do
      occList <- readRef occListRef
      debug $ "mergeEvents: upd: prio=" ++ show prio ++ "; total occs=" ++ show (length $ concatMap snd occList)
      when (not $ null occList) $ do
        writeRef occListRef []
        trigger $ concatMap snd $ sortBy (comparing fst) occList
  forM_ (zip [0::Int ..] evts) $ \(num, evt) ->
    listenToEvent key evt prio $ \occs -> do
      debug $ "mergeEvents: listen: noccs=" ++ show (length occs)
      modifyRef occListRef ((num, occs):)
      registerUpd prio upd
  return pullpush
  where
    prio = nextPrio $ maximum $ map evtPrio evts
    evtPrio (Evt p _) = p

emptyEvent :: Event a
emptyEvent = Evt (bottomPrio bottomLocation) $ return (return [], emptyPush)

filterE :: (a -> Bool) -> Event a -> Event a
filterE p = transformEvent (filter p)

stepClockE :: Event ()
stepClockE = Evt (bottomPrio bottomLocation) $ do
  clock <- getClock
  return (pure [()], clock)

dropStepE :: Event a -> SignalGen (Event a)
dropStepE ~(Evt evtprio evt) = Evt prio <$> do
  addPrep <- getPreparationAdder
  newNode $ do
    (result, trigger, key) <- newEventInit
    (getoccs, evtnotifier) <- evt
    liftIO $ addPrep $ listenToPush key evtnotifier $ do
      occs <- getoccs
      when (not $ null occs) $ trigger occs
    return result
  where
    prio = nextPrio evtprio

eventFromList :: [[a]] -> SignalGen (Event a)
eventFromList occs = behaviorToEvent <$> behaviorFromList (occs ++ repeat [])

scanE :: a -> Event (a -> a) -> SignalGen (Event a)
scanE initial evt@(~(Evt evtprio _)) = fmap (Evt prio) $ newNode $ do
  (pullpush, trigger, key) <- newEventInit
  ref <- newRef initial
  listenToEvent key evt prio $ \occs -> do
    debug $ "accumE: occs=" ++ show (length occs)
    oldVal <- readRef ref
    let _:vals = scanl (flip ($)) oldVal occs
    writeRef ref $ last vals
    trigger vals
  return pullpush
  where
    prio = nextPrio evtprio

mapAccumE :: s -> Event (s -> (s, a)) -> SignalGen (Event a)
mapAccumE initial evt@(~(Evt evtprio _)) = fmap (Evt prio) $ newNode $ do
  (myevt, trigger, key) <- newEventInit
  ref <- newRef initial
  listenToEvent key evt prio $ \occs -> do
    debug $ "mapAccumE: occs=" ++ show (length occs)
    oldVal <- readRef ref
    let (newVal, occs') = mapAccumL (flip ($)) oldVal occs
    writeRef ref $ newVal
    trigger occs'
  return myevt
  where
    prio = nextPrio evtprio

mapAccumEM :: s -> Event (s -> SignalGen (s, a)) -> SignalGen (Event a)
mapAccumEM initial evt = mdo
  e <- generatorE $ go <$> prevState <@> expandE evt
  state <- scanD initial (const . fst <$> e)
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
  (evt, trigger, key) <- newEventSG prio
  addToPrep <- getPreparationAdder
  handler <- liftIO $ fmap weakToLike $
    mkWeakWithKey key $ add trigger addToPrep occsVar
  liftIO $ listenToExternalEvent ee handler
  return evt
  where
    add trigger addToPrep occsVar occ = do
      firstTime <- modifyMVar occsVar $ \occs -> return (occ:occs, null occs)
      when firstTime $ addToPrep $ registerUpd prio $ do
        occs <- liftIO $ swapMVar occsVar []
        trigger $ reverse occs

    prio = bottomPrio bottomLocation

takeWhileE :: (a -> Bool) -> Event a -> SignalGen (Event a)
takeWhileE cond ~(Evt evtprio evt) = fmap (Evt prio) $ newNode $ do
  (push, trigger) <- liftIO $ newPush
  ref <- newRef $ error "takeWhileE"
  (evtPull, evtNot) <- evt
  subref <- newRef evtPull
  writeRef ref ([], Just subref)
  listenToPullPush (WeakKey subref) evtPull evtNot prio $ \occs -> do
    (_, eventRef) <- readRef ref
    when (isJust eventRef) $ do
      let !(occs', rest) = span cond occs
      when (not $ null occs') $ do
        modifyRef ref $ \(_, y) -> (occs', y)
        trigger
        registerFini $ modifyRef ref $ \(_, y) -> ([], y)
      when (not $ null rest) $ registerFini $ writeRef ref ([], Nothing)
  return (fst <$> readRef ref, push)
  where
    prio = nextPrio evtprio

-- | @delayE evt@ creates an event whose occurrences at step N
-- is equal to the ocurrences of @evt@ at step N-1.
delayE :: Event a -> SignalGen (Event a)
delayE evt = do
  occsS <- delayB [] $ eventToBehavior evt
  return $ flattenE $ occsS <@ stepClockE

----------------------------------------------------------------------
-- discretes

instance Functor Discrete where
  fmap = mapDiscrete

instance Applicative Discrete where
  pure = pureDiscrete
  (<*>) = apDiscrete

newDiscreteInit
  :: a
  -> Initialize ((Pull a, Push), a -> Run (), WeakKey)
newDiscreteInit initial = do
  ref <- newRef initial
  (push, trigger) <- liftIO newPush
  return ((readRef ref, push), discreteTrigger ref trigger, WeakKey ref)

newDiscreteSG
  :: a
  -> Priority
  -> SignalGen (Discrete a, Run a, a -> Run (), WeakKey)
newDiscreteSG initial prio = do
  ref <- newRef initial
  (push, trigger) <- liftIO newPush
  let dis = Dis prio $ return (readRef ref, push)
  return (dis, readRef ref, discreteTrigger ref trigger, WeakKey ref)

discreteTrigger :: IORef a -> Run () -> a -> Run ()
discreteTrigger buf notify val = do
  writeRef buf val
  notify

mapDiscrete :: (a -> b) -> Discrete a -> Discrete b
mapDiscrete f (Dis dprio dis) = Dis prio $ debugFrame "mapDiscrete" $ transparentMemoD $ do
  (pull, notifier) <- dis
  return (f <$> pull, notifier)
  where
    prio = nextPrio dprio

pureDiscrete :: a -> Discrete a
pureDiscrete value = Dis (bottomPrio bottomLocation) $
  return (pure value, emptyPush)

apDiscrete :: Discrete (a -> b) -> Discrete a -> Discrete b
-- both arguments must have been memoized
apDiscrete (Dis fprio fun) (Dis aprio arg)
    = Dis prio $ debugFrame "apDiscrete" $ unsafeCache $ do
  dirtyRef <- newRef False
  (pullpush, set, key) <- newDiscreteInit (error "apDiscrete: uninitialized")
  (funPull, funNot) <- fun
  (argPull, argNot) <- arg
  let
    upd = do
      debug $ "apDiscrete.upd; prio=" ++ show prio
      dirty <- readRef dirtyRef
      when dirty $ do
        writeRef dirtyRef False
        set =<< funPull <*> argPull
  let handler _ = do
        debug $ "apDiscrete.handler: prio=" ++ show prio
        writeRef dirtyRef True
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
  -> (a -> Run ())
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
  (push, trigger) <- liftIO newPush
  fmap (Dis prio) $ newNode $ do
    prio `shouldBeGreaterThan` outerprio
    runSubinit <- makeSubinitializer here
    listenToDiscrete (WeakKey outerRef) outer prio $ \inner -> do
      debug $ "joinDD: outer"
      innerRef <- newRef $ error "joinDD: innerRef not initialized"
      writeRef outerRef innerRef
      runSubinit $ do
        listenToDiscrete (WeakKey innerRef) inner prio $ \val -> do
          currentInnerRef <- readRef outerRef
          when (currentInnerRef == innerRef) $ do
            debug $ "joinDD: inner"
            writeRef innerRef val
            trigger
    return (readRef outerRef >>= readRef, push)

joinDE :: Discrete (Event a) -> SignalGen (Event a)
joinDE outer@ ~(Dis outerprio _) = do
  here <- genLocation
  let prio = bottomPrio here
  outerRef <- newRef $ error "joinDE: outerRef not initialized"
  (push, trigger) <- liftIO newPush
  fmap (Evt prio) $ newNode $ do
    prio `shouldBeGreaterThan` outerprio
    runSubinit <- makeSubinitializer here
    listenToDiscrete (WeakKey outerRef) outer prio $ \inner -> do
      debug $ "joinDE: outer"
      innerRef <- newRef []
      writeRef outerRef innerRef
      runSubinit $ do
        listenToEvent (WeakKey innerRef) inner prio $ \occs -> do
          currentInnerRef <- readRef outerRef
          when (currentInnerRef == innerRef) $ do
            debug $ "joinDE: inner noccs=" ++ show (length occs)
            writeRef innerRef occs
            registerFini $ writeRef innerRef []
            trigger
    return (readRef outerRef >>= readRef, push)

joinDB :: Discrete (Behavior a) -> SignalGen (Behavior a)
joinDB outer@ ~(Dis outerprio _) = do
  here <- genLocation
  let prio = bottomPrio here
  outerRef <- newRef $ error "joinDB: outerRef not initialized"
  fmap (Beh prio) $ newNode $ do
    prio `shouldBeGreaterThan` outerprio
    runSubinit <- makeSubinitializer here
    listenToDiscrete (WeakKey outerRef) outer prio
        $ \(Beh innerprio sig) -> do
      debug $ "joinDB: outer"
      pull <- runSubinit $ do
        prio `shouldBeGreaterThan` innerprio
        sig
      writeRef outerRef pull
    return (readRef outerRef >>= id)

----------------------------------------------------------------------
-- behaviors

instance Functor Behavior where
  fmap f (Beh prio pull) = Beh prio $ transparentMemoS $ fmap f <$> pull

instance Applicative Behavior where
  pure x = Beh (bottomPrio bottomLocation) (return $ pure x)
  Beh f_prio f_init <*> Beh a_prio a_init =
      Beh (max f_prio a_prio) $ transparentMemoS $ do
    f_pull <- f_init
    a_pull <- a_init
    return $ f_pull <*> a_pull

start :: SignalGen (Behavior a) -> IO (IO a)
start gensig = do
  ref <- newRef Nothing
  return $ runRun $ debugFrame "step" $ do
    state <- readRef ref
    case state of
      Nothing -> do
        (getval, prep) <- runSignalGenToplevel $ do
          Beh _ sig <- gensig
          return $ sig
        writeRef ref $ Just $ do
          debugFrame "prep" prep
          runUpdates
          debugFrame "getval" getval
        runUpdates
        debugFrame "getval" getval
      Just go -> go

externalB :: IO a -> SignalGen (Behavior a)
externalB get = fmap (Beh (bottomPrio bottomLocation)) $
  newNode $ primStepMemo (liftIO get)

joinB :: Behavior (Behavior a) -> SignalGen (Behavior a)
joinB ~(Beh _sigsigprio sigsig) = do
  here <- genLocation
  let prio = bottomPrio here
  fmap (Beh prio) $ newNode $ do
    debug $ "joinB: making pull; prio=" ++ show prio
    runSubinit <- makeSubinitializer here
    sigpull <- sigsig
    primStepMemo $ do
      Beh _sigprio sig <- sigpull
      pull <- runSubinit sig
      debugFrame "pull" pull

delayB :: a -> Behavior a -> SignalGen (Behavior a)
delayB initial ~(Beh sigprio sig) = do
  ref <- newRef initial
  registerInit $ do
    clock <- getClock
    pull <- sig
    listenToPush (WeakKey ref) clock $
      registerUpd (nextPrio sigprio) $ do
        debug "delayB: pull"
        newVal <- pull
        registerFini $ writeRef ref newVal
  return $ Beh prio $ return $ readRef ref
  where
    prio = bottomPrio bottomLocation

behaviorFromList :: [a] -> SignalGen (Behavior a)
behaviorFromList xs = debugFrame "behaviorFromList" $ do
  clock <- dropStepE stepClockE
  suffixD <- scanD xs $ drop 1 <$ clock
  return $ discreteToBehavior $ hd <$> suffixD
  where
    hd = fromMaybe (error "listtoBehavior: list exhausted") .
      listToMaybe

networkToList :: Int -> SignalGen (Behavior a) -> IO [a]
networkToList count network = do
  smp <- start network
  replicateM count smp

networkToListGC :: Int -> SignalGen (Behavior a) -> IO [a]
networkToListGC count network = do
  smp <- start network
  replicateM count (performGC >> smp)

snapshotB :: Behavior a -> SignalGen a
snapshotB ~(Beh sigprio sig) = do
  ienv <- getIEnv
  loc <- genLocation
  let prio = bottomPrio loc
  liftIO $ runInit ienv $ do
    prio `shouldBeGreaterThan` sigprio
    pull <- sig
    runInCurrentStep pull

----------------------------------------------------------------------
-- events and discretes

scanD :: a -> Event (a -> a) -> SignalGen (Discrete a)
scanD initial evt@(~(Evt evtprio _)) = fmap (Dis prio) $ newNode $ do
  (pullpush@(get, _), set, key) <- newDiscreteInit initial
  listenToEvent key evt prio $ \occs -> do
    debug $ "scanD: prio=" ++ show prio ++ "; occs=" ++ show (length occs)
    oldVal <- get
    set $! foldl' (flip ($)) oldVal occs
  return pullpush
  where
    prio = nextPrio evtprio

changesD :: Discrete a -> Event a
changesD (Dis disprio dis) = Evt prio $ unsafeCache $ do
  ref <- newRef []
  (disPull, disPush) <- dis
  let upd = eventTrigger ref (return ()) . (:[]) =<< disPull
  listenToPush (WeakKey ref) disPush upd
  runInCurrentStep $ do
    -- If we are in a step, we need to set up the ref now.
    active <- pushHasBeenTriggered disPush
    when active upd
  return (readRef ref, disPush)
  where
    prio = nextPrio disprio

preservesD :: Discrete a -> SignalGen (Event a)
preservesD dis@ ~(Dis disprio _) = fmap (Evt prio) $ newNode $ do
  (evt, trigger, key) <- newEventInit
  listenToDiscrete key dis prio $ \val -> trigger [val]
  return evt
  where
    prio = nextPrio disprio

delayD :: a -> Discrete a -> SignalGen (Discrete a)
delayD initial dis@ ~(Dis disprio _dis) = do
  (dis2, _get, set, key) <- newDiscreteSG initial (bottomPrio bottomLocation)
  registerInit $ do
    clock <- getClock
    listenToDiscrete key dis (nextPrio disprio) $ \val ->
      listenToPushOnce clock $ set val
  return dis2

----------------------------------------------------------------------
-- events and behaviors

eventToBehavior :: Event a -> Behavior [a]
eventToBehavior (Evt prio evt) = Beh prio $ do
  (pull, _push) <- evt
  return pull

behaviorToEvent :: Behavior [a] -> Event a
behaviorToEvent (Beh sigprio sig) = Evt prio $ unsafeCache $ do
  debug "behaviorToEvent"
  sigpull <- sig
  (pullpush, trigger, key) <- newEventInit
    --- ^ Here we create a fresh event, even though its pull component
    -- will be identical to sigpull. This is because we want a new key
    -- to keep the new notifier alive as long as the new pull, rather
    -- than the original pull, is alive.
  clock <- getClock
  listenToPullPush key (return ()) clock prio $ \_ ->
    registerUpd prio $ do
      occs <- sigpull
      debug $ "behaviorToEvent: onclock prio=" ++ show prio
        ++ "; noccs=" ++ show (length occs)
      when (not $ null occs) $ trigger occs
  return pullpush
  where
    prio = nextPrio sigprio

applyBE :: Behavior (a -> b) -> Event a -> Event  b
applyBE (Beh fprio fun) arg@(Evt aprio _)
    = Evt prio $ debugFrame "applyBE" $ unsafeCache $ do
  (pullpush, trigger, key) <- newEventInit
  funPull <- fun
  let
    upd occs = do
      debug $ "applyBE; prio=" ++ show prio
      funVal <- funPull
      trigger $ map funVal occs
  listenToEvent key arg prio $ \occs -> do
    debug $ "applyBE: prio=" ++ show prio
    registerUpd prio $ upd occs
  return pullpush
  where
    srcprio = max fprio aprio
    prio = nextPrio srcprio

----------------------------------------------------------------------
-- discretes and behaviors

discreteToBehavior :: Discrete a -> Behavior a
discreteToBehavior (Dis prio dis) = Beh prio $ fst <$> dis

----------------------------------------------------------------------
-- classes

class Functor s => TimeFunction s where
  toBehavior :: s a -> Behavior a

instance TimeFunction Behavior where
  toBehavior = id

instance TimeFunction Discrete where
  toBehavior = discreteToBehavior

infixl 4 <@> -- same as <$> and <*>

(<@>) :: (TimeFunction s) => s (a -> b) -> Event a -> Event b
f <@> a = applyBE (toBehavior f) a

(<@) :: (TimeFunction s) => s b -> Event a -> Event b
v <@ a = const <$> v <@> a

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

_unitTest :: IO ()
_unitTest = runTestThrow tests

tests :: Test
tests = test
  [ test_behaviorFromList
  , test_behaviorToEvent
  , test_scanD
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
  , test_joinB
  , test_delayB
  , test_generatorE
  , test_generatorE1
  , test_accumE
  , test_fmapBehavior
  , test_applyBE
  , test_joinDD
  , test_joinDE
  , test_joinDB
  , test_mfix
  , test_orderingViolation_joinDB
  , test_externalEvent
  , test_externalE
  , test_mapAccumE
  , test_mapAccumEM
  , test_mapAccumEquivalent
  , test_delayE
  , test_snapshotB
  ]

_skipped =
  [ test_takeWhileE -- broken when compiled. why???
  ]

test_behaviorFromList = do
  r <- networkToList 4 $ behaviorFromList ["foo", "bar", "baz", "quux", "xyzzy"]
  r @?= ["foo", "bar", "baz", "quux"]

test_behaviorToEvent = do
  r <- networkToList 3 $ do
    s0 <- behaviorFromList ["foo", "", "baz"]
    return $ eventToBehavior $ behaviorToEvent s0
  r @?= ["foo", "", "baz"]

test_scanD = do
  r <- networkToList 3 $ do
    strB <- behaviorFromList ["foo", "", "baz"]
    accD <- scanD "<>" $ append <$> behaviorToEvent strB
    return $ discreteToBehavior accD
  r @?= ["<>/'f'/'o'/'o'", "<>/'f'/'o'/'o'", "<>/'f'/'o'/'o'/'b'/'a'/'z'"]
  where
    append ch str = str ++ "/" ++ show ch

test_changesD = do
  r <- networkToList 3 $ do
    strB <- behaviorFromList ["foo", "", "baz"]
    accD <- scanD "<>" $ append <$> behaviorToEvent strB
    return $ eventToBehavior $ changesD accD
  r @?= [["<>/'f'/'o'/'o'"], [], ["<>/'f'/'o'/'o'/'b'/'a'/'z'"]]
  where
    append ch str = str ++ "/" ++ show ch

test_delayD = do
  r <- networkToList 5 $ do
    nS <- behaviorFromList (map pure $ iterate (+1) 0)
    nD <- scanD (0 :: Int) (const <$> behaviorToEvent nS)
    nD' <- delayD (-1) nD
    nE <- preservesD ((,) <$> nD <*> nD')
    return $ eventToBehavior nE
  r @?= map pure [(0, -1), (1, 0), (2, 1), (3, 2), (4, 3)]

test_mappendEvent = do
  r <- networkToListGC 3 $ do
    strB <- behaviorFromList ["foo", "", "baz"]
    accD <- scanD "<>" $ append <$> behaviorToEvent strB
    ch <- preservesD accD
    return $ eventToBehavior $
      ch `mappend` (behaviorToEvent $ (:[]) <$> strB)
  r @?= [["<>/'f'/'o'/'o'", "foo"], [""], ["<>/'f'/'o'/'o'/'b'/'a'/'z'", "baz"]]
  where
    append ch str = str ++ "/" ++ show ch

test_fmapEvent = do
  succCountRef <- newRef (0::Int)
  r <- networkToListGC 3 $ do
    strB <- behaviorFromList ["foo", "", "baz"]
    let lenE = mysucc succCountRef <$> behaviorToEvent strB
    return $ eventToBehavior $ lenE `mappend` lenE
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
    strB <- behaviorFromList ["FOo", "", "nom", "bAz"]
    let lenE = filterE Char.isUpper $ behaviorToEvent strB
    return $ eventToBehavior $ lenE `mappend` lenE
  r @?= ["FOFO", "", "", "AA"]

test_dropStepE = do
  r <- networkToListGC 3 $ do
    strB <- behaviorFromList ["foo", "", "baz"]
    lenE <- dropStepE $ behaviorToEvent strB
    return $ eventToBehavior $ lenE `mappend` lenE
  r @?= ["", "", "bazbaz"]

test_dropStepE1 = do
  r <- networkToListGC 3 $
    eventToBehavior <$> dropStepE stepClockE
  r @?= [[], [()], [()]]

test_apDiscrete = do
  r <- networkToListGC 4 $ do
    ev0 <- behaviorToEvent <$> behaviorFromList [[], [], [1::Int], [2,3]]
    ev1 <- behaviorToEvent <$> behaviorFromList [[], [4], [], [5]]
    dis0 <- scanD 0 $ max <$> ev0
    dis1 <- scanD 0 $ max <$> ev1
    let dis = (*) <$> dis0 <*> dis1
    eventToBehavior <$> preservesD dis
  r @?= [[0], [0], [4], [15]]

test_apDiscrete1 = do
  r <- networkToListGC 4 $ do
    ev0 <- eventFromList [[], [], [2::Int], [3,4]]
    ev1 <- eventFromList [[-1], [7], [], [11]]
    dis0 <- scanD 1 $ const <$> ev0
    dis1 <- scanD 1 $ const <$> ev1
    let dis = (*) <$> dis0 <*> dis1
    return $ discreteToBehavior dis
  r @?= [-1, 7, 14, 44]

test_eventFromList = do
  r <- networkToListGC 3 $ do
    ev <- eventFromList [[2::Int], [], [3,4]]
    return $ eventToBehavior ev
  r @?= [[2], [], [3,4]]

test_preservesD = do
  r <- networkToListGC 3 $ do
    ev <- eventFromList [[], [], [3,4::Int]]
    dis <- scanD 0 (const <$> ev)
    ev1 <- preservesD dis
    return $ eventToBehavior ev1
  r @?= [[0], [], [4]]

test_joinB = do
  r <- networkToListGC 4 $ do
    beh0 <- behaviorFromList [1, 2, 3, 4::Int]
    beh1 <- behaviorFromList [11, 12, 13, 14]
    beh2 <- behaviorFromList [21, 22, 23, 24]
    beh3 <- behaviorFromList [31, 32, 33, 34]
    behBeh <- behaviorFromList [beh0, beh3, beh2, beh1]
    joinB behBeh
  r @?= [1, 32, 23, 14]

test_delayB = do
  r <- networkToListGC 4 $ do
    beh <- behaviorFromList [1, 2, 3, 4::Int]
    delayB (-1) beh
  r @?= [-1, 1, 2, 3]

test_generatorE = do
  r <- networkToListGC 4 $ do
    evBeh <- generatorE =<< eventFromList [[subnet0], [subnet1], [subnet2], [subnet3]]
    let behBeh = head <$> eventToBehavior evBeh
    joinB behBeh
  r @?= [1, 11, 21, 31]
  where
    subnet0 = behaviorFromList [1, 2, 3, 4::Int]
    subnet1 = behaviorFromList [11, 12, 13, 14]
    subnet2 = behaviorFromList [21, 22, 23, 24]
    subnet3 = behaviorFromList [31, 32, 33, 34]

test_generatorE1 = do
  r <- networkToListGC 4 $ do
    evEv <- generatorE =<<
      eventFromList [[subnet 0], [subnet 1, subnet 2], [], [subnet 3]]
    dEv <- scanD mempty $ const <$> evEv
    ev <- joinDE dEv
    return $ eventToBehavior ev
  r @?= [[1], [21], [22, 23], [31]]
  where
    subnet k = fmap (10*k+) <$> eventFromList [[1], [2,3], [], [4::Int]]

test_accumE = do
  r <- networkToList 3 $ do
    strB <- behaviorFromList ["foo", "", "baz"]
    accE <- scanE "<>" $ append <$> behaviorToEvent strB
    return $ eventToBehavior accE
  r @?= [["<>f", "<>fo", "<>foo"], [], ["<>foob", "<>fooba", "<>foobaz"]]
  where
    append ch str = str ++ [ch]

test_fmapBehavior = do
  succCountRef <- newRef (0::Int)
  r <- networkToListGC 3 $ do
    chS <- behaviorFromList ['f', 'a', 'r']
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

test_applyBE = do
  r <- networkToListGC 3 $ do
    evt <- eventFromList ["ab", "", "c"]
    beh <- behaviorFromList [0, 1, 2::Int]
    return $ eventToBehavior $ (,) <$> beh <@> evt
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
      discreteToBehavior <$> joinDD outer

    discrete initial list = do
      evt <- eventFromList list
      scanD initial $ const <$> evt

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
      eventToBehavior <$> joinDE outer

    discrete initial list = do
      evt <- eventFromList list
      scanD initial $ const <$> evt

test_joinDB = do
  r <- networkToList 4 net
  r1 <- networkToListGC 4 net
  r @?= ["0a", "1b", "1c", "0d"]
  r1 @?= r
  where
    net = do
      inner0 <- behaviorFromList ["0a", "0b", "0c", "0d"]
      inner1 <- behaviorFromList ["1a", "1b", "1c", "1d"]
      outer <- discrete inner0 [[], [inner1], [], [inner0]]
      joinDB outer

    discrete initial list = do
      evt <- eventFromList list
      scanD initial $ const <$> evt

test_mfix = do
  r <- networkToList 3 net
  r @?= [1, 6, 30]
  where
    net = fmap snd $ mfix $ \ ~(e', _) -> do
      r <- scanD 1 $ (*) <$> e'
      e <- eventFromList [[], [2,3], [5::Int]]
      return (e, discreteToBehavior r)

test_orderingViolation_joinDB = do
  g <- start net
  g >>= (@?=(0::Int))
  g >>= (@?=1)
  shouldThrowOrderingViolation g
  where
    net = fmap snd $ mfix $ \ ~(sd', _) -> do
      s <- joinDB sd'
      se <- eventFromList [[], [pure 1], [s]]
      sd <- scanD (pure 0) $ const <$> se
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
  g <- start $ eventToBehavior <$> externalE ee
  triggerExternalEvent ee "b"
  g >>= (@?=[])
  triggerExternalEvent ee "c"
  g >>= (@?=["c"])
  g >>= (@?=[])
  triggerExternalEvent ee "d"
  triggerExternalEvent ee "e"
  g >>= (@?=["d","e"])

test_takeWhileE = do
  finalizerRecord <- newRef []
  inputRefA <- newRef []
  inputRefB <- newRef []
  let add ident = modifyRef finalizerRecord (ident:)
  wA <- mkWeakWithIORef inputRefA inputRefA (Just $ add "A")
  wB <- mkWeakWithIORef inputRefB inputRefB (Just $ add "B")
  g <- start $ do
    behA <- externalB $ readRef inputRefA
    behB <- externalB $ readRef inputRefB
    evtA <- takeWhileE (>0) $ behaviorToEvent behA
    evtB <- takeWhileE (>0) $ behaviorToEvent behB
    return $ (,) <$> eventToBehavior evtA <*> eventToBehavior evtB

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
  eventToBehavior <$> ac 0 ((\i s -> f (i + s, i + s :: Int)) <$> evt)

test_mapAccumE = do
  r <- mkAccumCount 10 mapAccumE id
  r @?= (take 10 $ map pure (iterate (+1) 1))

test_mapAccumEM = do
  r <- networkToList 16 $ do
    evt <- eventFromList $ map pure $ iterate (+1) 0
    eE <- mapAccumEM 0 ((\s n -> do e <- eventFromList (replicate n [] ++ [[n]]); return (n+s, e)) <$> evt)
    intE <- joinDE =<< scanD mempty (mappend <$> eE)
    return $ eventToBehavior intE
  r @?= [[0],[0],[],[1],[],[],[3],[],[],[],[6],[],[],[],[],[10]]

test_mapAccumEquivalent = do
  r1 <- mkAccumCount 10 mapAccumE id
  r2 <- mkAccumCount 10 mapAccumEM return
  r1 @?= r2

test_delayE = do
  r <- networkToList 4 $ do
    evt <- eventFromList ["ab", "", "c", "d"]
    eventToBehavior <$> delayE evt
  r @?= ["", "ab", "", "c"]

test_snapshotB = do
  r <- networkToList 6 $ do
    beh <- behaviorFromList [0::Int ..]
    b0 <- snapshotB beh
    ev <- eventFromList ["ab", "", "", "c", "", ""]
    let
      subnet ch = do
        v <- snapshotB beh
        if even v
          then return $ pure ""
          else do
            numB <- behaviorFromList [0::Int ..]
            return $ (show v++) . (ch:) . show <$> numB

    bE <- generatorE $ subnet <$> ev
    bD <- scanD (pure "") $ const <$> bE
    b <- joinB $ discreteToBehavior bD
    return $ (show b0++) <$> b
  r @?= ["0", "0", "0", "03c0", "03c1", "03c2"]

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
