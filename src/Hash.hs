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
    coreDisplay,
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
coreCompute input = do
    if length input == 16 then do
        modMatrix (doubleround10Compute input) input
    else
        error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The core expression as a string.
coreDisplay :: [String] -> [String]
coreDisplay input = do
    if length input == 16 then do
        modMatrixDisplay (doubleround10Display input) input
    else
        error "input to `core10Display` must be a list of 16 `String` strings"

-- |The core2 expression computed.
core2Compute :: [Word32] -> [Word32]
core2Compute input = do
    if length input == 16 then do
        modMatrix (doubleround2Compute input) input
    else
        error "input to `core2Compute` must be a list of 16 `Word32` numbers"

-- |The core2 expression as a string.
core2Display :: [String] -> [String]
core2Display input = do
    if length input == 16 then do
        modMatrixDisplay (doubleround2Display input) input
    else
        error "input to `core2Display` must be a list of 16 `String` strings"

-- |The core2 expression as a list of equations.
core2Equations :: [String] -> [String]
core2Equations input = do
    if length input == 16 then do
        let display = core2Display input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `core1Equations` must be a list of 16 `String` strings"

-- |The salsa20 expression computed.
salsa20Compute :: [Word32] -> [Word32]
salsa20Compute input = do
    if length input == 64 then do
        aument $ coreCompute $ reduce input
    else
        error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |The salsa20 expression as a string using `core2Display` which is only two rounds of doubleround.
salsa20Display :: [String] -> [String]
salsa20Display input = do
    if length input == 64 then do
        aumentDisplay $ core2Display $ reduceDisplay input
    else
        error "input to `salsa20Display` must be a list of 64 `String` strings"

-- |The salsa20 expression as a list of equations.
salsa20Equations :: [String] -> [String]
salsa20Equations input = do
    if length input == 64 then do
        let display = salsa20Display input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `salsa20Equations` must be a list of 64 `String` strings"

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case. 
salsa20powerCompute :: [Word32] -> Int -> [Word32]
salsa20powerCompute input 0 = input
salsa20powerCompute input power = salsa20powerCompute (salsa20Compute input) (power - 1)
