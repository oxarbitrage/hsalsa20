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
    columnround
    ) where

import Rowround (rowroundCompute)

import Data.Word

-- |Transpose a salsa20 matrix type.
transpose :: [Word32] -> [Word32]
transpose [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] = 
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transpose _ = []

-- |The columnround expression.
columnround :: [Word32] -> [Word32]
columnround = transpose . rowroundCompute . transpose
