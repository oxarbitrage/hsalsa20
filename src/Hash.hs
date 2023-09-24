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
    coreCompute,
    coreDisplay,
    core1Compute,
    core1Display,
    core1Equations,
    core2Compute,
    core2Display,
    core2Equations,
    salsa20Compute,
    salsa20Display,
    salsa20Equations,
    salsa20powerCompute,
    ) where

import Data.Word
import Text.Printf

import Doubleround
import Utils

-- |The core expression computed.
coreCompute :: [Word32] -> [Word32]
coreCompute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = modMatrix (doubleroundRCompute input 10) input
coreCompute _ = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The core expression as a string.
coreDisplay :: [String] -> [String]
coreDisplay input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = 
    modMatrixDisplay (doubleroundRDisplay input 10) input
coreDisplay _ = error "input to `coreDisplay` must be a list of 16 `String` strings"

-- |The core1 expression computed.
core1Compute :: [Word32] -> [Word32]
core1Compute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = modMatrix (doubleroundCompute input) input
core1Compute _ = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The core1 expression as a string.
core1Display :: [String] -> [String]
core1Display input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = 
    modMatrixDisplay (doubleroundDisplay input) input
core1Display _ = error "input to `core1Display` must be a list of 16 `String` strings"

-- |The core1 expression as a list of equations.
core1Equations :: [String] -> [String]
core1Equations input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
    [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (core1Display input)]
core1Equations _ = error "input to `core1Equations` must be a list of 16 `String` strings"

-- |The core2 expression computed.
core2Compute :: [Word32] -> [Word32]
core2Compute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = modMatrix (doubleroundRCompute input 2) input
core2Compute _ = error "input to `core2Compute` must be a list of 16 `Word32` numbers"

-- |The core2 expression as a string.
core2Display :: [String] -> [String]
core2Display input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = 
    modMatrixDisplay (doubleroundRDisplay input 2) input
core2Display _ = error "input to `core2Display` must be a list of 16 `String` strings"

-- |The core2 expression as a list of equations.
core2Equations :: [String] -> [String]
core2Equations input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
    [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (core2Display input)]
core2Equations _ = error "input to `core2Equations` must be a list of 16 `String` strings"

-- | The salsa20 expression computed.
salsa20Compute :: [Word32] -> [Word32]
salsa20Compute input
    | length input == 64 = aument $ coreCompute $ reduce input
    | otherwise = error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |The salsa20 expression as a string using `core1Display` which is only one round of doubleround.
salsa20Display :: [String] -> [String]
salsa20Display input 
    | length input == 64 = aumentDisplay $ core1Display $ reduceDisplay input
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
