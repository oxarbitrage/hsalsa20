{-|
Module      : Columnround
Description : Implementation of the Salsa20 stream cipher columnround expressions.
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module treats the columnround function as expressions analogous to `Rowround` with the input transposed.
The columnround function processes a 4x4 matrix, utilizing the rowround function with the transposed input.
The quarterround expressions are formed and evaluated through F-Algebra operations, including modular arithmetic,
rotation, and bitwise XOR.

The module offers functionalities to:

- Compute numeric values resulting from columnround expressions.
- Generate string representations of columnround expressions.
- Produce a list of equations corresponding to columnround expressions.
- Perform Keelung specific computations using the UInt 32 type.

The columnround function mirrors the rowround structure, providing consistency in the cryptographic operations.

-}
{-# LANGUAGE DataKinds #-}

module Columnround
    (
        columnroundCompute, columnroundDisplay, columnroundEquations, columnroundKeelung,
    )
where

import Rowround
import Utils

import Data.Word
import Text.Printf

import Keelung hiding (input, eq)

-- |The columnround expression computed.
columnroundCompute :: [Word32] -> [Word32]
columnroundCompute input
    | length input == 16 = transpose $ rowroundCompute $ transpose input
    | otherwise = error "input to `columnroundCompute` must be a list of 16 `Word32` numbers"

-- |The columnround expression as a string.
columnroundDisplay :: [String] -> [String]
columnroundDisplay input
    | length input == 16 = transpose $ rowroundDisplay $ transpose input
    | otherwise = error "input to `columnroundDisplay` must be a list of 16 `String` strings"

-- |The columnround expression as a list of equations.
columnroundEquations :: [String] -> [String]
columnroundEquations input
    | length input == 16 = [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (columnroundDisplay input)]
    | otherwise = error "input to `columnroundEquations` must be a list of 16 `String` strings"

-- |The Keelung columnround expression.
columnroundKeelung :: [UInt 32] -> Comp [UInt 32]
columnroundKeelung input
    | length input == 16 = do
        let new_input = transpose input
        k <- rowroundKeelung new_input
        return $ transpose k
    | otherwise = error "input to `columnroundKeelung` must be a list of 16 `UInt 32` numbers"
