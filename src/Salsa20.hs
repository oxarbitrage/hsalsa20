module Salsa20
    (
    salsa20,
    salsa20power
    ) where

import Doubleround

import Utils

import Types (MatrixType, Matrix64Type)

--
salsa20 :: Matrix64Type -> Matrix64Type
salsa20 = aument . core . reduce

--
core :: MatrixType -> MatrixType
core input = modMatrix (doubleround10 input) input

--
salsa20power :: Matrix64Type -> Int -> Matrix64Type
salsa20power input 0 = input
salsa20power input power =  salsa20power (salsa20 input) (power - 1)
