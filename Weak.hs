{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- WARNING
-- This file causes a segfault if interpreted with ghci.
-- If you use ghci, compile this file first and then
-- load the .o file to ghci.

module Weak (mkWeakWithIORef) where

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
  case mkWeak# ref v (unsafeCoerce# 0#) s of
    (# s1, w #) -> (# s1, Weak w #)

-- vim: sw=2 ts=2 sts=2
