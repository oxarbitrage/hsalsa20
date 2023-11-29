{-|
Module      : Doubleround
Description : Implementation of the Salsa20 stream cipher doubleround expressions.
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module defines the doubleround function as the composition of rowround and columnround. 
Additionally, it introduces `doubleroundR`, which represents the `doubleround` function applied R times,
as specified in the salsa20 spec.

The module provides functionalities to:

- Compute numeric values resulting from doubleround expressions.
- Generate string representations of doubleround expressions.
- Perform Keelung specific computations using the UInt 32 type.

The doubleround function applies rowround to the result of columnround, creating a comprehensive cryptographic operation.
The iterated version, doubleroundR, repeats the doubleround operation R times, enhancing security and complexity.

Exported functions:

- 'doubleroundCompute': Computes numeric values for a doubleround expression.
- 'doubleroundDisplay': Generates string representations of a doubleround expression.
- 'doubleroundKeelung': Performs Keelung-specific computations for a doubleround expression.

The 'doubleroundR' variant:

- 'doubleroundRCompute': Computes numeric values for an iterated doubleround expression.
- 'doubleroundRDisplay': Generates string representations of an iterated doubleround expression.
- 'doubleroundRKeelung': Performs Keelung-specific computations for an iterated doubleround expression.

-}
{-# LANGUAGE DataKinds #-}

module Doubleround
    (
        doubleroundCompute, doubleroundDisplay, doubleroundKeelung,
        doubleroundRCompute, doubleroundRDisplay, doubleroundRKeelung,
    )
where

import Data.Word

import Keelung hiding (input, eq)

import Rowround
import Columnround

-- |The doubleround expression computed.
{-@ doubleroundCompute :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 }  @-}
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute input
    | length input == 16 = rowroundCompute $ columnroundCompute input
    | otherwise = error "input to `doubleroundCompute` must be a list of 16 `Word32` numbers"

-- |The doubleround expression as a string.
{-@ doubleroundDisplay :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 }  @-}
doubleroundDisplay :: [String] -> [String]
doubleroundDisplay input
    | length input == 16 = rowroundDisplay $ columnroundDisplay input
    | otherwise = error "input to `doubleroundDisplay` must be a list of 16 `String` strings"

-- |The doubleround Keelung expression.
{-@ doubleroundKeelung :: { i:[_] | (len i) == 16 } -> Comp { o:[_] | (len o) == 16 }  @-}
doubleroundKeelung :: [UInt 32] -> Comp [UInt 32]
doubleroundKeelung input
    | length input == 16 = rowroundKeelung =<< columnroundKeelung input
    | otherwise = error "input to `doubleroundKeelung` must be a list of 16 `UInt 32` numbers"

-- |The doubleroundR expression computed.
{-@ doubleroundRCompute ::  Nat -> { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
doubleroundRCompute :: Int -> [Word32] -> [Word32]
doubleroundRCompute r input
    | length input == 16 && r == 0 = input
    | length input == 16 && r > 0 = doubleroundRCompute (r - 1) (doubleroundCompute input)
    | otherwise = error "arguments of `doubleroundRCompute` must be a number `Int` of rounds and a list of 16 `Word32` numbers"

-- |The doubleroundR expression as a string.
{-@ doubleroundRDisplay ::  Nat -> { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
doubleroundRDisplay ::  Int -> [String] -> [String]
doubleroundRDisplay r input
    | length input == 16 && r == 0 = input
    | length input == 16 && r > 0 = doubleroundRDisplay (r - 1) (doubleroundDisplay input)
    | otherwise = error "arguments of `doubleroundRDisplay` must be a number `Int` of rounds and a list of 16 `String` strings"

-- |The doubleroundR Keelung expression.
{-@ doubleroundRKeelung :: Nat -> { i:[_] | (len i) == 16 } -> Comp { o:[_] | (len o) == 16 }  @-}
doubleroundRKeelung :: Int -> [UInt 32] -> Comp [UInt 32]
doubleroundRKeelung r input
    | length input == 16 && r == 0 = return input
    | length input == 16 && r > 0 = doubleroundKeelung input >>= \dr -> doubleroundRKeelung (r - 1) dr
    | otherwise = error "arguments of `doubleroundRKeelung` must be a list of 16 `UInt 32` numbers and a number `Int` of rounds"
