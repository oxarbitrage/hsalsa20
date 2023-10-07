{-|
Module      : Columnround
Description : Columnround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We treat the columnround just as `Rowround` expressions with the input transposed.
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
