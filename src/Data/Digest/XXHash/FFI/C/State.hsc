{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}

-- |
-- Module:      Data.Digest.XXHash.FFI.C.State
-- Copyright:   (c) 2018 Henri Verroken, Steven Keuchel
-- Licence:     BSD3
-- Maintainer:  Henri Verroken <henriverroken@gmail.com
-- Stability:   stable
-- Portability: GHC
--
module Data.Digest.XXHash.FFI.C.State
( -- * 32-bit state
  XXH32State
, allocaXXH32State

  -- * 64-bit state
, XXH64State
, allocaXXH64State

) where

import GHC.Exts (Int(..), MutableByteArray##, RealWorld, newByteArray##)
import GHC.IO   (IO(IO))

-- Define XXH_STATIC_LINKING_ONLY to expose the definition of the state structs.
-- We can then get the size of them and allocate them on the managed GHC heap.
#define XXH_STATIC_LINKING_ONLY
#include "xxhash.h"

-- | Intermediate state for computing a XXH32 using segmentation or streams.
type XXH32State = MutableByteArray## RealWorld

-- | Intermediate state for computing a XXH64 using segmentation or streams.
type XXH64State = MutableByteArray## RealWorld

{-# INLINE allocaMutableByteArray #-}
allocaMutableByteArray :: Int -> (MutableByteArray## RealWorld -> IO b) -> IO b
allocaMutableByteArray (I## len) f = IO $ \s0 ->
    case newByteArray## len s0 of { (## s1, mba ##) ->
    case f mba                 of { IO m -> m s1 }}

{-# INLINE allocaXXH32State #-}
-- | 'allocaXXH32State f' temporarily allocates a 'XXH32State' and passes it
--   to the function 'f'.
allocaXXH32State :: (XXH32State -> IO a) -> IO a
allocaXXH32State = allocaMutableByteArray #{size XXH32_state_t}

{-# INLINE allocaXXH64State #-}
-- | 'allocaXXH64State f' temporarily allocates a 'XXH64State' and passes it
--   to the function 'f'.
allocaXXH64State :: (XXH64State -> IO a) -> IO a
allocaXXH64State = allocaMutableByteArray #{size XXH64_state_t}
