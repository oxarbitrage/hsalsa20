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
    doubleroundTypeChecker,
    doubleround10Compute,
    --doubleround10TypeChecker,
    ) where

import Data.Word

import Rowround (rowroundCompute, rowroundTypeChecker, stringList2numbers)
import Columnround (columnroundCompute, columnroundTypeChecker)

-- |The doubleround expression.
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute = rowroundCompute . columnroundCompute

tupleToList22 :: [(Word32, String)] -> [String]
tupleToList22 = map snd

-- |The doubleround expression.
--doubleroundTypeChecker :: [Word32] -> [String]
doubleroundTypeChecker :: [Word32] -> [(Word32, String)]
doubleroundTypeChecker input = rowroundTypeChecker ( stringList2numbers (tupleToList22 (columnroundTypeChecker input) ))

-- |The doubleround10 expression.
doubleround10Compute :: [Word32] -> [Word32]
doubleround10Compute = doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute 
    . doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute 
    . doubleroundCompute

-- |The doubleround10 expression.
--doubleround10TypeChecker :: [Word32] -> [String]
--doubleround10TypeChecker = doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker 
--    . doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker 
--    . doubleroundTypeChecker . doubleroundTypeChecker
