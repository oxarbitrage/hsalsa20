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
- 'coreEquations': Produces a list of equations for the core expression.
- 'coreKeelung': Performs Keelung-specific computations for the core expression.

- 'salsa20Compute': Computes numeric values for the salsa20 expression.
- 'salsa20Display': Generates string representations of the salsa20 expression.
- 'salsa20Equations': Produces a list of equations for the salsa20 expression.
- 'salsa20Keelung': Performs Keelung-specific computations for the salsa20 expression.

- 'salsa20powerCompute': Executes the salsa20 function a specified number of times.

-}
{-# LANGUAGE DataKinds #-}

module Hash
    (
        coreCompute, coreDisplay, coreEquations, coreKeelung,
        salsa20Compute, salsa20Display, salsa20Equations, salsa20Keelung,
        salsa20powerCompute,
    )
where

import Data.Word
import Text.Printf

import Keelung hiding (input, eq)

import Doubleround
import Utils

-- |The core expression computed.
coreCompute :: [Word32] -> Int -> [Word32]
coreCompute input rounds
    | length input == 16 = modMatrix (doubleroundRCompute input rounds) input
    | otherwise = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- |The core expression as a string.
coreDisplay :: [String] -> Int -> [String]
coreDisplay input rounds
    | length input == 16 = modMatrixDisplay (doubleroundRDisplay input rounds) input
    | otherwise = error "input to `coreDisplay` must be a list of 16 `String` strings"

-- |The core expression as a list of equations.
coreEquations :: [String] -> Int -> IO ()
coreEquations input rounds
    | length input == 16 = mapM_ putStrLn $ [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (coreDisplay input rounds)]
    | otherwise = error "input to `core2Equations` must be a list of 16 `String` strings"

-- |The core Keelung expression computed.
coreKeelung :: [UInt 32] -> Int -> Comp [UInt 32]
coreKeelung input rounds
    | length input == 16 = do
        dr <- doubleroundRKeelung input rounds
        return $ modMatrixKeelung dr input
    | otherwise = error "input to `coreCompute` must be a list of 16 `Word32` numbers"

-- | The salsa20 expression computed.
salsa20Compute :: [Word32] -> [Word32]
salsa20Compute input
    | length input == 64 = aument $ coreCompute (Utils.reduce input) 10
    | otherwise = error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |The salsa20 expression as a string using `core1Display` which is only one round of doubleround.
salsa20Display :: [String] -> [String]
salsa20Display input 
    | length input == 64 = aumentDisplay $ coreDisplay (reduceDisplay input) 10
    | otherwise = error "input to `salsa20Display` must be a list of 64 `String` strings"

-- |The salsa20 expression as a list of equations.
salsa20Equations :: [String] -> IO ()
salsa20Equations input
    | length input == 64 = mapM_ putStrLn $ [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (salsa20Display input)]
    | otherwise = error "input to `salsa20Equations` must be a list of 64 `String` strings"

-- | The salsa20 Keelung expression computed.
salsa20Keelung :: [UInt 32] -> Comp [UInt 32]
salsa20Keelung input
    | length input == 64 = do
        let new_input = reduceKeelung input
        core <- coreKeelung new_input 10
        return $ aumentKeelung core
    | otherwise = error "input to `salsa20Compute` must be a list of 64 `Word32` numbers"

-- |Execute `salsa20` a specified number of times, this is not part of the protocol and just used in a test case. 
salsa20powerCompute :: [Word32] -> Int -> [Word32]
salsa20powerCompute input 0 = input
salsa20powerCompute input power = salsa20powerCompute (salsa20Compute input) (power - 1)
