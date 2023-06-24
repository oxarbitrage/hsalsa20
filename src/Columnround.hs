{-|
Module      : Columnround
Description : Columnround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We treat the columnround just as `Rowround` expressions with the input transposed.
-}
module Columnround
    (
    columnroundCompute,
    columnroundDisplay,
    columnroundEquations,
    ) where

import Rowround
import Utils

import Data.Word
import Text.Printf

-- |The columnround expression computed.
columnroundCompute :: [Word32] -> [Word32]
columnroundCompute input = do
    if length input == 16 then
        transpose $ rowroundCompute $ transpose input
    else
        error "input to `columnroundCompute` must be a list of 16 `Word32` numbers"

-- |The columnround expression as a string.
columnroundDisplay :: [String] -> [String]
columnroundDisplay input = do
    if length input == 16 then
        transpose $ rowroundDisplay $ transpose input
    else
        error "input to `columnroundDisplay` must be a list of 16 `String` strings"

-- |The columnround expression as a list of equations.
columnroundEquations :: [String] -> [String]
columnroundEquations input = do
    if length input == 16 then do
        let display = columnroundDisplay input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `columnroundEquations` must be a list of 16 `String` strings"
