{-|
Module      : Expansion
Description : Salsa20 expansion function code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module implements the Salsa20 expansion function, which is an essential part of the Salsa20 stream cipher.
The expansion function derives the internal key schedule from the provided key and nonce, preparing the cipher for
 data encryption and decryption.

There are two variants of the expansion function based on the key size:

1. 'expand32': Used with 32-byte (256-bit) keys.
2. 'expand16': Used with 16-byte (128-bit) keys.

Each of these functions takes a key and nonce as input and produces the Salsa20 keystream as output.
The 32-byte version is the most commonly used variant in practice.

The module provides functions for computing, displaying, and generating equations for both 'expand32' and
'expand16' operations. The internal order and constants used in these functions are also defined within this module
for clarity.

-}
module Expansion
    (
        expand32Compute, expand32Display,
        expand16Compute, expand16Display,
    )
where

import Data.Word

import Hash
import Utils

-- |`expa` in ascii.
{-@ o0 :: { o:[_] | (len o) == 4 } @-}
o0 :: [Word32]
o0 = [101, 120, 112, 97]

-- |`nd 3` in ascii.
{-@ o1 :: { o:[_] | (len o) == 4 } @-}
o1 :: [Word32]
o1 = [110, 100, 32, 51]

-- |`2-by` in ascii.
{-@ o2 :: { o:[_] | (len o) == 4 } @-}
o2 :: [Word32]
o2 = [50, 45, 98, 121]

-- |`te k` in ascii.
{-@ o3 :: { o:[_] | (len o) == 4 } @-}
o3 :: [Word32]
o3 = [116, 101, 32, 107]

-- |`expa` in ascii. 
{-@ t0 :: { o:[_] | (len o) == 4 } @-}
t0 :: [Word32]
t0 = [101, 120, 112, 97]

-- |`nd 1` in ascii.
{-@ t1 :: { o:[_] | (len o) == 4 } @-}
t1 :: [Word32]
t1 = [110, 100, 32, 49]

-- |`6-by` in ascii.
{-@ t2 :: { o:[_] | (len o) == 4 } @-}
t2 :: [Word32]
t2 = [54, 45, 98, 121]

-- |`te k` in ascii.
{-@ t3 :: { o:[_] | (len o) == 4 } @-}
t3 :: [Word32]
t3 = [116, 101, 32, 107]

-- | The expansion function where we have two 16 bytes k's (k0 and k1).
{-@ expand32Compute :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
expand32Compute :: [Word32] -> [Word32] -> [Word32] -> [Word32]
expand32Compute k0 k1 n
    | length k0 == 16 && length k1 == 16 && length n == 16 = salsa20Compute 10 (sort32Compute k0 k1 n)
    | otherwise = error "inputs to `expand32Compute` must be 2 lists of 16 `Word32` numbers as k0 and k1; and a \
        \list of 16 `Word32` numbers as an `n`"

-- |The expansion function displayed where we have two 16 bytes (k0 and k1).
{-@ expand32Display :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
expand32Display :: [String] -> [String] -> [String] -> [String]
expand32Display k0 k1 n
    | length k0 == 16 && length k1 == 16 && length n == 16 = salsa20Display 10 (sort32Display k0 k1 n)
    | otherwise = error "inputs to `expand32Display` must be 2 lists of 16 `String` strings as a k0 and k1; and a \
        \list of 16 `String` strings as an `n`"

-- |The order needed for the 32 bytes k version of the expansion function `expand32`.
{-@ sort32Compute :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
sort32Compute :: [Word32] -> [Word32] -> [Word32] -> [Word32]
sort32Compute k0 k1 n = o0 ++ k0 ++ o1 ++ n ++ o2 ++ k1 ++ o3 

-- |The order needed for the 32 bytes k version of the expansion function `expand32`.
{-@ sort32Display :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
sort32Display :: [String] -> [String] -> [String] -> [String]
sort32Display k0 k1 n = numberListToStringList o0 ++ k0 ++ numberListToStringList o1 ++ n ++
    numberListToStringList o2 ++ k1 ++ numberListToStringList o3

-- |The expansion function computed where we have one 16 bytes (k).
{-@ expand16Compute :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
expand16Compute :: [Word32] -> [Word32] -> [Word32]
expand16Compute k n
    | length k == 16 && length n == 16 = salsa20Compute 10 (sort16Compute k n)
    | otherwise = error "inputs to `expand16Compute` must be a list of 16 `Word32` numbers as a key and a \
        \list of 16 `Word32` numbers as an `n`"

-- |The expansion function displayed where we have one 16 bytes (k).
{-@ expand16Display :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
expand16Display :: [String] -> [String] -> [String]
expand16Display k n
    | length k == 16 && length n == 16 = salsa20Display 10 (sort16Display k n)
    | otherwise = error "inputs to `expand16Display` must be a list of 16 `String` strings as a key and a \
        \list of 16 `String` strings as an `n`"

-- |Expand for computation of the 16 bytes k version of the expansion function `expand16`.
{-@ sort16Compute :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
sort16Compute :: [Word32] -> [Word32] -> [Word32]
sort16Compute k n = t0 ++ k ++ t1 ++ n ++ t2 ++ k ++ t3

-- |Expand for display of the 16 bytes k version of the expansion function `expand16`.
{-@ sort16Display :: { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 16 } -> { o:[_] | (len o) == 64 } @-}
sort16Display :: [String] -> [String] -> [String]
sort16Display k n = numberListToStringList t0 ++ k ++ numberListToStringList t1 ++ n ++
    numberListToStringList t2 ++ k ++ numberListToStringList t3
