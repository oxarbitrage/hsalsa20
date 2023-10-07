{-|
Module      : Doubleround
Description : Doubleround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We define the doubleround function as the composition of rowround and columnround.

In addition we define `doubleroundR`, which is the `doubleround` function applied R times,
as specified in the salsa20 spec.
-}
{-# LANGUAGE DataKinds #-}

module Doubleround
    (
        doubleroundCompute, doubleroundDisplay, doubleroundEquations, doubleroundKeelung,
        doubleroundRCompute, doubleroundRDisplay, doubleroundREquations, doubleroundRKeelung,
    )
where

import Data.Word
import Text.Printf

import Keelung hiding (input, eq)

import Rowround
import Columnround

-- |The doubleround expression computed.
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute input
    | length input == 16 = rowroundCompute $ columnroundCompute input
    | otherwise = error "input to `doubleroundCompute` must be a list of 16 `Word32` numbers"

-- |The doubleround expression as a string.
doubleroundDisplay :: [String] -> [String]
doubleroundDisplay input
    | length input == 16 = rowroundDisplay $ columnroundDisplay input
    | otherwise = error "input to `doubleroundDisplay` must be a list of 16 `String` strings"

-- |The doubleround expression as a list of equations.
doubleroundEquations :: [String] -> [String]
doubleroundEquations input
    | length input == 16 = [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (doubleroundDisplay input)]
    | otherwise = error "input to `doubleroundEquations` must be a list of 16 `String` strings"

-- |The doubleround Keelung expression.
doubleroundKeelung :: [UInt 32] -> Comp [UInt 32]
doubleroundKeelung input
    | length input == 16 = rowroundKeelung =<< columnroundKeelung input
    | otherwise = error "input to `doubleroundKeelung` must be a list of 16 `UInt 32` numbers"

-- |The doubleroundR expression computed.
doubleroundRCompute :: [Word32] -> Int -> [Word32]
doubleroundRCompute input r
    | length input == 16 && r == 0 = input
    | length input == 16 && r > 0 = doubleroundRCompute (doubleroundCompute input) (r - 1)
    | otherwise = error "arguments of `doubleroundRCompute` must be a list of 16 `Word32` numbers and a number `Int` of rounds"

-- |The doubleroundR expression as a string.
doubleroundRDisplay :: [String] -> Int -> [String]
doubleroundRDisplay input r
    | length input == 16 && r == 0 = input
    | length input == 16 && r > 0 = doubleroundRDisplay (doubleroundDisplay input) (r - 1)
    | otherwise = error "arguments of `doubleroundRDisplay` must be a list of 16 `String` strings and a number `Int` of rounds"

-- |The doubleroundR expression as a list of equations.
doubleroundREquations :: [String] -> Int -> [String]
doubleroundREquations input r
    | length input == 16 && r == 0 = input
    | length input == 16 && r > 0 = [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (doubleroundRDisplay input r)]
    | otherwise = error "arguments of `doubleroundREquations` must be a list of 16 `String` strings and a number `Int` of rounds"

-- |The doubleroundR Keelung expression.
doubleroundRKeelung :: [UInt 32] -> Int -> Comp [UInt 32]
doubleroundRKeelung input r
    | length input == 16 && r == 0 = return input
    | length input == 16 && r > 0 = doubleroundKeelung input >>= \dr -> doubleroundRKeelung dr (r - 1)
    | otherwise = error "arguments of `doubleroundRKeelung` must be a list of 16 `UInt 32` numbers and a number `Int` of rounds"
