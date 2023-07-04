{-|
Module      : Doubleround
Description : Doubleround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We define the doubleround function as the composition of rowround and columnround.

In addition we define `doubleround10`, which is the `doubleround` function applied 10 times,
as specified in the salsa20 spec and `doubleround2` which is the `doubleround` function
applied 2 times only for practical analysis of expressions purposes.
-}
module Doubleround
    (
    doubleroundCompute,
    doubleroundDisplay,
    doubleroundEquations,
    doubleround10Compute,
    doubleround10Display,
    doubleround10Equations,
    doubleround2Compute,
    doubleround2Display,
    doubleround2Equations,
    ) where

import Data.Word
import Text.Printf

import Rowround
import Columnround
import Utils

-- |The doubleround expression computed.
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute input = do
    if length input == 16 then do
        rowroundCompute $ columnroundCompute input
    else
        error "input to `doubleroundCompute` must be a list of 16 `Word32` numbers"

-- |The doubleround expression as a string.
doubleroundDisplay :: [String] -> [String]
doubleroundDisplay input = do
    if length input == 16 then do
        rowroundDisplay (columnroundDisplay input)
    else
        error "input to `doubleroundDisplay` must be a list of 16 `String` strings"

-- |The doubleround expression as a list of equations.
doubleroundEquations :: [String] -> [String]
doubleroundEquations input = do
    if length input == 16 then do
        let display = doubleroundDisplay input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `doubleroundEquations` must be a list of 16 `String` strings"

-- |The doubleround10 expression computed.
doubleround10Compute :: [Word32] -> [Word32]
doubleround10Compute input = do
    if length input == 16 then do
        doubleroundCompute $ doubleroundCompute $ doubleroundCompute $ doubleroundCompute $ 
            doubleroundCompute $ doubleroundCompute $ doubleroundCompute $ doubleroundCompute $
            doubleroundCompute $ doubleroundCompute input
    else
        error "input to `doubleround10Compute` must be a list of 16 `Word32` numbers"

-- |The doubleround10 expression as a string.
doubleround10Display :: [String] -> [String]
doubleround10Display input = do
    if length input == 16 then do
        doubleroundDisplay $ doubleroundDisplay $ doubleroundDisplay $ doubleroundDisplay $
            doubleroundDisplay $ doubleroundDisplay $ doubleroundDisplay $ doubleroundDisplay $
            doubleroundDisplay $ doubleroundDisplay input
    else
        error "input to `doubleround10Display` must be a list of 16 `String` strings"

-- |The doubleround10 expression as a list of equations.
doubleround10Equations :: [String] -> [String]
doubleround10Equations input = do
    if length input == 16 then do
        let display = doubleround10Display input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `doubleround10Equations` must be a list of 16 `String` strings"

-- |The doubleround2 expression computed.
doubleround2Compute :: [Word32] -> [Word32]
doubleround2Compute input = do
    if length input == 16 then do
        doubleroundCompute $ doubleroundCompute input
    else
        error "input to `doubleround2Compute` must be a list of 16 `Word32` numbers"

-- |The doubleround2 expression as a string.
doubleround2Display :: [String] -> [String]
doubleround2Display input = do
    if length input == 16 then do
        doubleroundDisplay $ doubleroundDisplay input
    else
        error "input to `doubleround2Display` must be a list of 16 `String` strings"

-- |The doubleround10 expression as a list of equations.
doubleround2Equations :: [String] -> [String]
doubleround2Equations input = do
    if length input == 16 then do
        let display = doubleround2Display input
        let displayIndex = zip [index0..] display
        let equation = map (uncurry (printf "z%d = %s")) displayIndex
        equation
    else
        error "input to `doubleround2Equations` must be a list of 16 `String` strings"
