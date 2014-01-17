{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- WARNING
-- This file causes a segfault if interpreted with ghci < 7.6.
-- If you use ghci, compile this file first and then
-- load the .o file to ghci.

module FRP.Ordrea.Weak (mkWeakWithIORef) where

import GHC.Exts
import GHC.IO
import GHC.IORef
import GHC.STRef
import GHC.Weak

mkWeakWithIORef :: IORef a -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakWithIORef (IORef (STRef ref)) v (Just fin) = IO $ \s ->
  case mkWeak# ref v fin s of
    (# s1, w #) -> (# s1, Weak w #)
mkWeakWithIORef (IORef (STRef ref)) v Nothing = IO $ \s ->
  case mkWeakNoFinalizer# ref v s of
    (# s1, w #) -> (# s1, Weak w #)

# if __GLASGOW_HASKELL__ < 706
mkWeakNoFinalizer#
  :: MutVar# RealWorld a
  -> v
  -> State# RealWorld
  -> (# State# RealWorld, Weak# v #)
mkWeakNoFinalizer# key val s = mkWeak# key val (unsafeCoerce# 0#) s
# endif

-- vim: sw=2 ts=2 sts=2
