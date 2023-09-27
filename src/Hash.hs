{-|
Module      : Hash
Description : Salsa20 hash function
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

The salsa20 `core` function and what we call the `hash` expressions.

The `hash` expression is the extended `core` expression. It is formed by reducing the input from 64
to 16 using `littleendian` functions and then aument the result back to 64.
-}
module Hash
    (
        coreCompute, coreDisplay, coreEquations, salsa20Compute, salsa20Display, salsa20Equations,
        salsa20powerCompute,
    )
where

import Data.Word
import Text.Printf

import Doubleround
import Utils

-- |The core expression computed.
coreCompute :: [Word32] -> Int -> [Word32]
coreCompute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] rounds = modMatrix (doubleroundRCompute input rounds) input
coreCompute _ _ = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The core expression as a string.
coreDisplay :: [String] -> Int -> [String]
coreDisplay input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] rounds =
    modMatrixDisplay (doubleroundRDisplay input rounds) input
coreDisplay _ _ = error "input to `coreDisplay` must be a list of 16 `String` strings"

-- |The core expression as a list of equations.
coreEquations :: [String] -> Int -> [String]
coreEquations input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] rounds =
    [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (coreDisplay input rounds)]
coreEquations _ _ = error "input to `core2Equations` must be a list of 16 `String` strings"

-- | The salsa20 expression computed.
salsa20Compute :: [Word32] -> [Word32]
salsa20Compute input
    | length input == 64 = aument $ coreCompute (reduce input) 10
    | otherwise = error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |The salsa20 expression as a string using `core1Display` which is only one round of doubleround.
salsa20Display :: [String] -> [String]
salsa20Display input 
    | length input == 64 = aumentDisplay $ coreDisplay (reduceDisplay input) 10
    | otherwise = error "input to `salsa20Display` must be a list of 64 `String` strings"

-- |The salsa20 expression as a list of equations.
salsa20Equations :: [String] -> [String]
salsa20Equations input
    | length input == 64 = [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (salsa20Display input)]
    | otherwise = error "input to `salsa20Equations` must be a list of 64 `String` strings"

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case. 
salsa20powerCompute :: [Word32] -> Int -> [Word32]
salsa20powerCompute input 0 = input
salsa20powerCompute input power = salsa20powerCompute (salsa20Compute input) (power - 1)
