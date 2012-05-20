{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Ordrea where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Mem.Weak

type Consumer a = a -> IO ()

listenToPush :: Push a -> Priority -> (a -> Run ()) -> key -> Initialize ()
listenToPush push prio handler key = do
  weak <- liftIO $ mkWeak key handler Nothing
  register <- push
  liftIO $ register prio weak

type Pull a = Run a
type Push a = Initialize (Priority -> Weak (a -> Run ()) -> IO ())

newtype SignalGen a = SignalGen (ReaderT GEnv IO a)
  deriving (Monad, Functor, Applicative, MonadIO)
type Initialize = ReaderT IEnv IO
type Run = ReaderT REnv IO
type Finalize = IO

-- Location of a dynamic node.
type Location = U.Vector Word

data GEnv = GEnv
  { envRegisterInit :: Consumer (Initialize ())
  , envGenLocation :: IO Location
  }

data IEnv = IEnv
  { envClock :: Push ()
  , envParentLocation :: Location
  , envRegisterFirstStep :: Consumer (Run ())
  }

data REnv = REnv
  { envRegisterFini :: Consumer (Finalize ())
  }

-- Priority of updates.
data Priority = Priority
  { priLoc :: {-- UNPACK #-} !Location
  , priNum :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord) -- The default lexicographical ordering is appropriate


data Signal a   = Sig {-# UNPACK #-} !Priority !(Pull a)
data Event a    = Evt {-# UNPACK #-} !Priority !(Pull [a]) !(Push a)
data Discrete a = Dis {-# UNPACK #-} !Priority !(Pull a) !(Push a)

-- TODO: specialize
newActionAccum :: (MonadIO m) => IO (Consumer (m ()), m ())
newActionAccum = do
  actions <- newRef []
  return (add actions, run actions)
  where
    add ref act = modifyIORef ref (act:)
    run ref = readRef ref >>= sequence_

newLocationGen :: Location -> IO (IO Location)
newLocationGen parentLoc = do
  counter <- newRef 0
  return $ do
    num <- readRef counter
    writeRef counter $! num + 1
    return $! parentLoc `U.snoc` num

runSignalGen :: Location -> Push () -> SignalGen a -> Run a
runSignalGen parentLoc clock (SignalGen gen) = do
  (registerI, runAccumI) <- liftIO newActionAccum
  (registerF, runAccumF) <- liftIO newActionAccum
  locGen <- liftIO $ newLocationGen parentLoc
  let
    genv = GEnv
      { envRegisterInit = registerI
      , envGenLocation = locGen
      }
    ienv = IEnv
      { envClock = clock
      , envRegisterFirstStep = registerF
      , envParentLocation = parentLoc
      }
  result <- liftIO $ runReaderT gen genv
  liftIO $ runReaderT runAccumI ienv
  runAccumF
  return result

runRun :: Run a -> IO a
runRun run = do
  (registerF, runAccumF) <- liftIO newActionAccum
  let renv = REnv { envRegisterFini = registerF }
  result <- runReaderT run renv
  runAccumF
  return result

bottomLocation :: Location
bottomLocation = U.empty

genLocation :: SignalGen Location
genLocation = SignalGen $ do
  gen <- asks envGenLocation
  lift gen

registerInit :: Initialize () -> SignalGen ()
registerInit ini = SignalGen $ do
  reg <- asks envRegisterInit
  lift $ reg ini

registerFirstStep :: Run () -> Initialize ()
registerFirstStep fst = do
  reg <- asks envRegisterFirstStep
  lift $ reg fst

registerFini :: IO () -> Run ()
registerFini fini = do
  reg <- asks envRegisterFini
  lift $ reg fini

newCachedPull :: Run a -> SignalGen (Pull a)
newCachedPull calc = do
  ref <- newRef Nothing
  return $ do
    cache <- readRef ref
    case cache of
      Just val -> return val
      Nothing -> do
        val <- calc
        writeRef ref (Just val)
        registerFini $ writeRef ref Nothing
        return val

bottomPrio :: Location -> Priority
bottomPrio loc = Priority
  { priLoc = loc
  , priNum = 0
  }

getClock :: Initialize (Push ())
getClock = asks envClock

gask :: SignalGen GEnv
gask = SignalGen ask

joinS :: Signal (Signal a) -> SignalGen (Signal a)
joinS ~(Sig _sigsigprio sigsig) = do
  here <- genLocation
  let prio = bottomPrio here
  pull <- newCachedPull $ do
    Sig _sigprio sig <- sigsig
    sig
  return $! Sig prio pull

delayS :: a -> Signal a -> SignalGen (Signal a)
delayS initial ~(Sig _sigprio sig) = do
  ref <- newRef initial
  registerInit $ do
    clock <- getClock
    listenToPush clock prio (upd ref) ref
  return $ Sig prio $ readRef ref
  where
    upd ref () = do
      newVal <- sig
      registerFini $ writeRef ref newVal
    prio = bottomPrio bottomLocation

newPush :: IO (Push a, a -> Run ())
newPush = do
  listenersRef <- newRef M.empty
  return (return $ register listenersRef, invoke listenersRef)
  where
    register ref listenerPrio listenerWeak = do
      listenerMap <- readRef ref
      writeRef ref $! M.alter (add listenerWeak) listenerPrio listenerMap
    add weak = Just . (weak:) . fromMaybe []

    invoke ref value = do
      m <- readRef ref
      m' <- M.fromList . catMaybes <$> mapM run (M.toList m)
      writeRef ref m'
      where
        run (prio, weaks) = do
          weaks' <- catMaybes <$> mapM run1 weaks
          return $! if null weaks' then Nothing else Just (prio, weaks')
        run1 weak = do
          m <- liftIO $ deRefWeak weak
          case m of
            Just consumer -> do
              consumer value
              return $ Just weak
            Nothing -> return Nothing

newEvent :: Priority -> SignalGen (Event a, a -> Run ())
newEvent prio = do
  ref <- newRef []
  (push, trigger) <- liftIO newPush
  registerInit $
    listenToPush push (bottomPrio bottomLocation) (add ref) ref
  let evt = Evt prio (reverse <$> readRef ref) push
  return (evt, trigger)
  where
    add ref val = do
      occs <- readRef ref
      writeRef ref (val:occs)
      when (null occs) $
        registerFini $ writeRef ref []

generatorE :: Event (SignalGen a) -> SignalGen (Event a)
generatorE evt = do
  here <- genLocation
  let prio = bottomPrio here
  (result, trigger) <- newEvent prio
  registerInit $ do
    clock <- getClock
    listenToEvent evt prio (handler here clock trigger) result
  return $ result
  where
    handler here clock trigger gen =
      trigger =<< runSignalGen here clock gen

listenToEvent :: Event a -> Priority -> (a -> Run ()) -> key -> Initialize ()
listenToEvent (Evt evtPrio evtPull evtPush) prio handler key = do
  listenToPush evtPush prio handler key
  parLoc <- asks envParentLocation
  when (priLoc evtPrio < parLoc) $
    registerFirstStep $ do
      initialOccs <- evtPull
      mapM_ handler initialOccs

start :: SignalGen (Signal a) -> IO (IO a)
start gensig = do
  (clock, clockTrigger) <- newPush
  Sig _ sig <- runRun $ runSignalGen bottomLocation clock gensig
  return $ runRun $ do
    clockTrigger ()
    sig

eventToSignal :: Event a -> Signal [a]
eventToSignal (Evt prio pull _) = Sig prio pull

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

-- vim: sw=2 ts=2 sts=2
