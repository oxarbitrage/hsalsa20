{-|
Module      : Hash
Description : Implementation of the Salsa20 hash function using the core function and hash expressions.
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module encapsulates the Salsa20 `core` function and introduces what is referred to as the `hash` or `salsa20` 
expressions.

The `core` function performs cryptographic operations by applying the `doubleround` function iteratively for a 
specified number of rounds.
The `salsa20` expression extends the `core` expression by reducing the input from 64 to 16 using `littleendian` functions
and then augmenting the result back to 64.

Exported functions:

- 'coreCompute': Computes numeric values for the core expression.
- 'coreDisplay': Generates string representations of the core expression.
- 'coreKeelung': Performs Keelung-specific computations for the core expression.

- 'salsa20Compute': Computes numeric values for the salsa20 expression.
- 'salsa20Display': Generates string representations of the salsa20 expression.
- 'salsa20Keelung': Performs Keelung-specific computations for the salsa20 expression.

- 'salsa20powerCompute': Executes the salsa20 function a specified number of times.

-}
{-# LANGUAGE DataKinds #-}

module Hash
    (
        coreCompute, coreDisplay, coreKeelung,
        salsa20Compute, salsa20Display, salsa20Keelung,
        salsa20powerCompute,
    )
where

import Data.Word

import Keelung hiding (input, eq)

import Doubleround
import Utils

-- |The core expression computed.
{-@ coreCompute ::  Nat -> { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
coreCompute :: Int -> [Word32] -> [Word32]
coreCompute rounds input
    | length input == 16 = modMatrix (doubleroundRCompute rounds input) input
    | otherwise = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The core expression as a string.
{-@ coreDisplay ::  Nat -> { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
coreDisplay :: Int -> [String] -> [String]
coreDisplay rounds input
    | length input == 16 = modMatrixDisplay (doubleroundRDisplay rounds input) input
    | otherwise = error "input to `coreDisplay` must be a list of 16 `String` strings"

-- |The core Keelung expression computed.
{-@ coreKeelung ::  Nat -> { i:[_] | (len i) == 16 } -> Comp { o:[_] | (len o) == 16 } @-}
coreKeelung :: Int -> [UInt 32] -> Comp [UInt 32]
coreKeelung rounds input
    | length input == 16 && rounds >= 0 = do
        dr <- doubleroundRKeelung rounds input
        return $ modMatrixKeelung dr input
    | otherwise = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- | The salsa20 expression computed.
{-@ salsa20Compute ::  Nat -> { i:[_] | (len i) == 64 } -> { o:[_] | (len o) == 64 } @-}
salsa20Compute :: Int -> [Word32] -> [Word32]
salsa20Compute rounds input
    | length input == 64 && rounds >= 0 = aument $ coreCompute rounds (Utils.reduce input)
    | otherwise = error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |The salsa20 expression as a string using `coreDisplay`. Call with r = 1, which is one round of doubleround.
{-@ salsa20Display ::  Nat -> { i:[_] | (len i) == 64 } -> { o:[_] | (len o) == 64 } @-}
salsa20Display :: Int -> [String] -> [String]
salsa20Display rounds input
    | length input == 64 && rounds >= 0 = aumentDisplay $ coreDisplay rounds (reduceDisplay input)
    | otherwise = error "input to `salsa20Display` must be a list of 64 `String` strings"

-- | The salsa20 Keelung expression computed.
--{-@ salsa20Keelung ::  { i:[_] | (len i) == 64 } -> Comp { o:[_] | (len o) == 64 } @-}
{-@ ignore salsa20Keelung @-}
salsa20Keelung :: [UInt 32] -> Comp [UInt 32]
salsa20Keelung input
    | length input == 64 = do
        let new_input = reduceKeelung input
        core <- coreKeelung 10 new_input
        return $ aumentKeelung core
    | otherwise = error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case.
{-@ ignore salsa20powerCompute @-}
salsa20powerCompute :: [Word32] -> Int -> [Word32]
salsa20powerCompute input 0 = input
salsa20powerCompute input power = salsa20powerCompute (salsa20Compute 10 input) (power - 1)
