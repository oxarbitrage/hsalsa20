{-|
Module      : Hash
Description : Salsa20 hash function
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

The salsa20 hash expressions.
-}
module Hash
    (
    coreCompute,
    salsa20Compute,
    salsa20powerCompute,
    ) where

import Data.Word

import Doubleround
import Utils

-- |The salsa20 expression.
salsa20Compute :: [Word32] -> [Word32]
salsa20Compute = aument . coreCompute . reduce

-- |The core of the salsa20.
coreCompute :: [Word32] -> [Word32]
coreCompute input = modMatrix (doubleround10Compute input) input

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case. 
salsa20powerCompute :: [Word32] -> Int -> [Word32]
salsa20powerCompute input 0 = input
salsa20powerCompute input power = salsa20powerCompute (salsa20Compute input) (power - 1)
