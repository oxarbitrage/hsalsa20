{-|
Module      : Doubleround
Description : Doubleround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

Here we define the doubleround function as the composition of rowround and columnround. 
In addition we define `doubleround10`, which is the `doubleround` function applied 10 times,
as specified in the salsa20 spec.
-}
module Doubleround
    (
    doubleroundCompute,
    doubleroundDisplay,
    doubleround10Compute,
    doubleround10Display,
    doubleround2Compute,
    doubleround2Display,
    ) where

import Data.Word

import Rowround
import Columnround

-- |The doubleround expression computed.
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute = rowroundCompute . columnroundCompute

-- |The doubleround expression as a string.
doubleroundDisplay :: [String] -> [String]
doubleroundDisplay input = rowroundDisplay (columnroundDisplay input)

-- |The doubleround10 expression computed.
doubleround10Compute :: [Word32] -> [Word32]
doubleround10Compute =
    doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute .
    doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute .
    doubleroundCompute . doubleroundCompute

-- |The doubleround10 expression as a string.
doubleround10Display :: [String] -> [String]
doubleround10Display =
    doubleroundDisplay . doubleroundDisplay  . doubleroundDisplay . doubleroundDisplay .
    doubleroundDisplay . doubleroundDisplay . doubleroundDisplay . doubleroundDisplay .
    doubleroundDisplay . doubleroundDisplay

-- |The doubleround2 expression computed.
doubleround2Compute :: [Word32] -> [Word32]
doubleround2Compute = doubleroundCompute . doubleroundCompute

-- |The doubleround2 expression as a string.
doubleround2Display :: [String] -> [String]
doubleround2Display = doubleroundDisplay . doubleroundDisplay
