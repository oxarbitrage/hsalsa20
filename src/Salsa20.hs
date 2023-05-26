{-|
Module      : Salsa20
Description : Salsa20 related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

The salsa20 expression.
-}
module Salsa20
    (
    salsa20,
    salsa20power
    ) where

import Doubleround

import Utils

import Types (MatrixType, Matrix64Type)

-- |The salsa20 expression.
salsa20 :: Matrix64Type -> Matrix64Type
salsa20 = aument . core . reduce

-- |The core of the salsa20.
core :: MatrixType -> MatrixType
core input = modMatrix (doubleround10 input) input

-- |Execute `salsa20` a number of time, this is not part of the protocol and just used in a test case. 
salsa20power :: Matrix64Type -> Int -> Matrix64Type
salsa20power input 0 = input
salsa20power input power =  salsa20power (salsa20 input) (power - 1)
