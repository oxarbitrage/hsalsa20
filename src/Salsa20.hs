module Salsa20
    (
    salsa20
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
