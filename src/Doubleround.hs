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
    doubleround,
    doubleround10
    ) where

import Data.Word

import Rowround (rowroundCompute)
import Columnround (columnroundCompute)

-- |The doubleround expression.
doubleround :: [Word32] -> [Word32]
doubleround = rowroundCompute . columnroundCompute

-- |The doubleround10 expression.
doubleround10 :: [Word32] -> [Word32]
doubleround10 = doubleround . doubleround . doubleround . doubleround . doubleround
    . doubleround . doubleround . doubleround . doubleround . doubleround
