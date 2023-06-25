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
    core1Display,
    core1Equations,
    salsa20Compute,
    salsa20powerCompute,
    ) where

import Data.Word
import Text.Printf

import Doubleround
import Utils

-- |The salsa20 expression.
salsa20Compute :: [Word32] -> [Word32]
salsa20Compute = aument . coreCompute . reduce

-- |The salsa20 core expression computed.
coreCompute :: [Word32] -> [Word32]
coreCompute input = do
    if length input == 16 then do
        modMatrix (doubleround10Compute input) input
    else
        error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The salsa20 core1 expression as a string.
core1Display :: [String] -> [String]
core1Display input = do
    if length input == 16 then do
        modMatrixDisplay (doubleroundDisplay input) input
    else
        error "input to `core1Display` must be a list of 16 `String` strings"

-- |The salsa20 core1 expression as a list of equations.
core1Equations :: [String] -> [String]
core1Equations input = do
    if length input == 16 then do
        let display = core1Display input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `doubleroundEquations` must be a list of 16 `String` strings"

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case. 
salsa20powerCompute :: [Word32] -> Int -> [Word32]
salsa20powerCompute input 0 = input
salsa20powerCompute input power = salsa20powerCompute (salsa20Compute input) (power - 1)
