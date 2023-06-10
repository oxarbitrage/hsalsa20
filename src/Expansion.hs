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
    expand32,
    expand16
    ) where

import Data.Word

import Hash (salsa20)

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

-- |The expansion function where we have two 16 bytes k's (k0 and k1).
expand32 :: [Word32] -> [Word32] -> [Word32] -> [Word32]
expand32 k0 k1 n = salsa20 $ sort32 k0 k1 n

-- |The order needed for the 32 bytes k version of the expansion function `expand32`.
sort32 :: [Word32] -> [Word32] -> [Word32] -> [Word32]
sort32 k0 k1 n = o0 ++ k0 ++ o1 ++ n ++ o2 ++ k1 ++ o3 

-- |The expansion function where we have one 16 bytes (k).
expand16 :: [Word32] -> [Word32] -> [Word32]
expand16 k n = salsa20 $ sort16 k n

-- |The order needed for the 16 bytes k version of the expansion function `expand16`.
sort16 :: [Word32] -> [Word32] -> [Word32]
sort16 k n = t0 ++ k ++ t1 ++ n ++ t2 ++ k ++ t3 
