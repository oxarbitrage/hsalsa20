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
    salsa20,
    salsa20power
    ) where

import Data.Word

import Doubleround
import Utils

-- |The salsa20 expression.
salsa20 :: [Word32] -> [Word32]
salsa20 = aument . core . reduce

-- |The core of the salsa20.
core :: [Word32] -> [Word32]
core input = modMatrix (doubleround10Compute input) input

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case. 
salsa20power :: [Word32] -> Int -> [Word32]
salsa20power input 0 = input
salsa20power input power = salsa20power (salsa20 input) (power - 1)
