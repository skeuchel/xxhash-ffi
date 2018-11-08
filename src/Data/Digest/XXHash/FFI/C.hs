{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module:      Data.Digest.XXHash.FFI.C
-- Copyright:   (c) 2017 Henri Verroken
-- Licence:     BSD3
-- Maintainer:  Henri Verroken <henriverroken@gmail.com
-- Stability:   stable
-- Portability: GHC
--
-- This module provides FFI imports to the C reference library at
-- <https://github.com/Cyan4973/xxHash>.
--
-- This binding keeps the intermediate state for stream processing in an
-- 'MutableByteArray#' on the managed GHC heap. All foreign imports use unsafe
-- call semantics. Therefore, it is possible to use either unpinned or pinned
-- 'MutableByteArray#' since GHC's garbage collector doesn't move the either
-- kind during an unsafe foreign call. However GHCi <8.4 may replace unsafe
-- foreign calls with safe foreign calls in the bytecode
-- interpreter. Consequently, unpinned 'MutableByteArray#s' may be moved by the
-- garbage collector during foreign calls which obviously breaks this code. So
-- extra care should be taken when loading this code into the bytecode
-- interpreter.
module Data.Digest.XXHash.FFI.C (
  -- * C Interface
  -- ** Direct Calculation
  c_xxh64
, c_xxh32

  -- ** 32-bit state functions
, XXH32State
, allocaXXH32State
, c_xxh32_copyState
, c_xxh32_reset
, c_xxh32_update
, c_xxh32_digest

  -- ** 64-bit state functions
, XXH64State
, allocaXXH64State
, c_xxh64_copyState
, c_xxh64_reset
, c_xxh64_update
, c_xxh64_digest
) where

import Data.Digest.XXHash.FFI.C.State
import Foreign.C.Types
import Foreign.Ptr       (Ptr)

foreign import ccall unsafe "XXH64" c_xxh64 ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CULLong    -- ^ Seed
 -> IO CULLong -- ^ Resulting hash

foreign import ccall unsafe "XXH32" c_xxh32 ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CUInt      -- ^ Seed
 -> IO CUInt   -- ^ Resulting hash

foreign import ccall unsafe "XXH32_copyState" c_xxh32_copyState ::
    XXH32State     -- ^ Destination
 -> XXH32State     -- ^ Source
 -> IO ()

foreign import ccall unsafe "XXH32_reset" c_xxh32_reset ::
    XXH32State     -- ^ The state to reset
 -> CUInt          -- ^ The initial seed
 -> IO ()

foreign import ccall unsafe "XXH32_update" c_xxh32_update ::
    XXH32State     -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

foreign import ccall unsafe "XXH32_digest" c_xxh32_digest ::
    XXH32State     -- ^ The state to digest
 -> IO CUInt       -- ^ Resulting hash

foreign import ccall unsafe "XXH64_copyState" c_xxh64_copyState ::
    XXH64State     -- ^ Destination
 -> XXH64State     -- ^ Source
 -> IO ()

foreign import ccall unsafe "XXH64_reset" c_xxh64_reset ::
    XXH64State     -- ^ The state to reset
 -> CULLong        -- ^ The initial seed
 -> IO ()

foreign import ccall unsafe "XXH64_update" c_xxh64_update ::
    XXH64State     -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

foreign import ccall unsafe "XXH64_digest" c_xxh64_digest ::
    XXH64State     -- ^ The state to digest
 -> IO CULLong     -- ^ Resulting hash