{-|
Module      : Expansion
Description : Salsa20 expansion function code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

The two forms of expansion supported by Salsa20 are given by key size.
-}
module Expansion
    (
    expand32Compute,
    expand32Display,
    expand32Equations,
    expand16Compute,
    expand16Display,
    expand16Equations,
    sort32Compute,
    ) where

import Data.Word
import Text.Printf

import Hash
import Utils

-- |`expa` in ascii. 
o0 :: [Word32]
o0 = [101, 120, 112, 97]

-- |`nd 3` in ascii.
o1 :: [Word32]
o1 = [110, 100, 32, 51]

-- |`2-by` in ascii.
o2 :: [Word32]
o2 = [50, 45, 98, 121]

-- |`te k` in ascii.
o3 :: [Word32]
o3 = [116, 101, 32, 107]

-- |`expa` in ascii. 
t0 :: [Word32]
t0 = [101, 120, 112, 97]

-- |`nd 1` in ascii.
t1 :: [Word32]
t1 = [110, 100, 32, 49]

-- |`6-by` in ascii.
t2 :: [Word32]
t2 = [54, 45, 98, 121]

-- |`te k` in ascii.
t3 :: [Word32]
t3 = [116, 101, 32, 107]

-- | The expansion function where we have two 16 bytes k's (k0 and k1).
expand32Compute :: [Word32] -> [Word32] -> [Word32] -> [Word32]
expand32Compute k0 k1 n
    | length k0 == 16 && length k1 == 16 && length n == 16 = salsa20Compute $ sort32Compute k0 k1 n
    | otherwise = error "inputs to `expand32Compute` must be 2 lists of 16 `Word32` numbers as k0 and k1; and a \
        \list of 16 `Word32` numbers as an `n`"

-- |The expansion function displayed where we have two 16 bytes (k0 and k1).
expand32Display :: [String] -> [String] -> [String] -> [String]
expand32Display k0 k1 n
    | length k0 == 16 && length k1 == 16 && length n == 16 = salsa20Display $ sort32Display k0 k1 n
    | otherwise = error "inputs to `expand32Display` must be 2 lists of 16 `String` strings as a k0 and k1; and a \
        \list of 16 `String` strings as an `n`"

-- |The expansion function equations where we have two 16 bytes (k0 and k1).
expand32Equations :: [String] -> [String] -> [String] -> [String]
expand32Equations k0 k1 n
    | length k0 == 16 && length k1 == 16 && length n == 16 =
        [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (expand32Display k0 k1 n)]
    | otherwise = error "inputs to `expand32Equations` must be 2 lists of 16 `String` strings as a k0 and k1;  and a \
        \list of 16 `String` strings as an `n`"

-- |The order needed for the 32 bytes k version of the expansion function `expand32`.
sort32Compute :: [Word32] -> [Word32] -> [Word32] -> [Word32]
sort32Compute k0 k1 n = o0 ++ k0 ++ o1 ++ n ++ o2 ++ k1 ++ o3 

-- |The order needed for the 32 bytes k version of the expansion function `expand32`.
sort32Display :: [String] -> [String] -> [String] -> [String]
sort32Display k0 k1 n = numberListToStringList o0 ++ k0 ++ numberListToStringList o1 ++ n ++
    numberListToStringList o2 ++ k1 ++ numberListToStringList o3

-- |The expansion function computed where we have one 16 bytes (k).
expand16Compute :: [Word32] -> [Word32] -> [Word32]
expand16Compute k n
    | length k == 16 && length n == 16 = salsa20Compute $ sort16Compute k n
    | otherwise = error "inputs to `expand16Compute` must be a list of 16 `Word32` numbers as a key and a \
        \list of 16 `Word32` numbers as an `n`"

-- |The expansion function displayed where we have one 16 bytes (k).
expand16Display :: [String] -> [String] -> [String]
expand16Display k n
    | length k == 16 && length n == 16 = salsa20Display $ sort16Display k n
    | otherwise = error "inputs to `expand16Display` must be a list of 16 `String` strings as a key and a \
        \list of 16 `String` strings as an `n`"

-- |The expansion function equations where we have one 16 bytes (k).
expand16Equations :: [String] -> [String] -> [String]
expand16Equations k n
    | length k == 16 && length n == 16 =
        [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (expand16Display k n)]
    | otherwise = error "inputs to `expand16Equations` must be a list of 16 `String` strings as a key and a \
        \list of 16 `String` strings as an `n`"

-- |Expand for computation of the 16 bytes k version of the expansion function `expand16`.
sort16Compute :: [Word32] -> [Word32] -> [Word32]
sort16Compute k n = t0 ++ k ++ t1 ++ n ++ t2 ++ k ++ t3 

-- |Expand for display of the 16 bytes k version of the expansion function `expand16`.
sort16Display :: [String] -> [String] -> [String]
sort16Display k n = numberListToStringList t0 ++ k ++ numberListToStringList t1 ++ n ++
    numberListToStringList t2 ++ k ++ numberListToStringList t3 
