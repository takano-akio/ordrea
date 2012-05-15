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

newtype IEnv = IEnv
  { envClock :: Push ()
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
data Event a    = Evt {-# UNPACK #-} !Priority !(Push a)
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

runSignalGen :: Location -> Push () -> SignalGen a -> IO a
runSignalGen parentLoc clock (SignalGen gen) = do
  (register, runAccum) <- newActionAccum
  locGen <- newLocationGen parentLoc
  let
    genv = GEnv
      { envRegisterInit = register
      , envGenLocation = locGen
      }
    ienv = IEnv
      { envClock = clock
      }
  result <- runReaderT gen genv
  runReaderT runAccum ienv
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

newPush :: SignalGen (Push a, a -> Run ())
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

generatorE :: Event (SignalGen a) -> SignalGen (Event a)
generatorE (Evt _evtprio evt) = do
  here <- genLocation
  let prio = bottomPrio here
  (push, trigger) <- newPush
  registerInit $ do
    clock <- getClock
    listenToPush evt prio (handler here clock trigger) here
  return $ Evt prio push
  where
    handler here clock trigger gen =
      trigger =<< liftIO (runSignalGen here clock gen)

----------------------------------------------------------------------
-- utils

newRef :: (MonadIO m) => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: (MonadIO m) => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: (MonadIO m) => IORef a -> a -> m ()
writeRef x v = liftIO $ writeIORef x v

-- vim: sw=2 ts=2 sts=2
