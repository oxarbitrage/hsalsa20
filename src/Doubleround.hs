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

import Types (MatrixType)
import Rowround (rowroundCompute)
import Columnround (columnround)

-- |The doubleround expression.
doubleround :: MatrixType -> MatrixType
doubleround = rowroundCompute . columnround

-- |The doubleround10 expression.
doubleround10 :: MatrixType -> MatrixType
doubleround10 = doubleround . doubleround . doubleround . doubleround . doubleround
    . doubleround . doubleround . doubleround . doubleround . doubleround
