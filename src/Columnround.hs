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
    ) where

import Rowround
import Utils

import Data.Word

-- |The columnround expression computed.
columnroundCompute :: [Word32] -> [Word32]
columnroundCompute = transpose . rowroundCompute . transpose

-- |The columnround expression as a string.
columnroundDisplay :: [String] -> [String]
columnroundDisplay = transpose . rowroundDisplay . transpose
