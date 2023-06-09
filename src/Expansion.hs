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

import Types (MatrixType, Matrix64Type, VectorType)
import Data.Tuple.Select

import Hash (salsa20)

-- |`expa` in ascii. 
o0 :: VectorType
o0 = (101, 120, 112, 97)

-- |`nd 3` in ascii.
o1 :: VectorType
o1 = (110, 100, 32, 51)

-- |`2-by` in ascii.
o2 :: VectorType
o2 = (50, 45, 98, 121)

-- |`te k` in ascii.
o3 :: VectorType
o3 = (116, 101, 32, 107)

-- |`expa` in ascii. 
t0 :: VectorType
t0 = (101, 120, 112, 97)

-- |`nd 1` in ascii.
t1 :: VectorType
t1 = (110, 100, 32, 49)

-- |`6-by` in ascii.
t2 :: VectorType
t2 = (54, 45, 98, 121)

-- |`te k` in ascii.
t3 :: VectorType
t3 = (116, 101, 32, 107)

-- |The expansion function where we have two 16 bytes k's (k0 and k1).
expand32 :: MatrixType -> MatrixType -> MatrixType -> Matrix64Type
expand32 k0 k1 n = salsa20 $ sort32 k0 k1 n

-- |The order needed for the 32 bytes k version of the expansion function `expand32`.
sort32 :: MatrixType -> MatrixType -> MatrixType -> Matrix64Type
sort32 k0 k1 n = (
    (o0, sel1 k0, sel2 k0, sel3 k0),
    (sel4 k0, o1, sel1 n, sel2 n),
    (sel3 n, sel4 n, o2, sel1 k1),
    (sel2 k1, sel3 k1, sel4 k1, o3))

-- |The expansion function where we have one 16 bytes (k).
expand16 :: MatrixType -> MatrixType -> Matrix64Type
expand16 k n = salsa20 $ sort16 k n

-- |The order needed for the 16 bytes k version of the expansion function `expand16`.
sort16 :: MatrixType -> MatrixType -> Matrix64Type
sort16 k n = (
    (t0, sel1 k, sel2 k, sel3 k),
    (sel4 k, t1, sel1 n, sel2 n),
    (sel3 n, sel4 n, t2, sel1 k),
    (sel2 k, sel3 k, sel4 k, t3))
