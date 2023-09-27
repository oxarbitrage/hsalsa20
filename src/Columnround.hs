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
        columnroundCompute, columnroundDisplay, columnroundEquations,
    )
where

import Rowround
import Utils

import Data.Word
import Text.Printf

-- |The columnround expression computed.
columnroundCompute :: [Word32] -> [Word32]
columnroundCompute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
    transpose $ rowroundCompute $ transpose input
columnroundCompute _ = error "input to `columnroundCompute` must be a list of 16 `Word32` numbers"

-- |The columnround expression as a string.
columnroundDisplay :: [String] -> [String]
columnroundDisplay input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
    transpose $ rowroundDisplay $ transpose input
columnroundDisplay _ = error "input to `columnroundDisplay` must be a list of 16 `String` strings"

-- |The columnround expression as a list of equations.
columnroundEquations :: [String] -> [String]
columnroundEquations input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
    [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (columnroundDisplay input)]
columnroundEquations _ = error "input to `columnroundEquations` must be a list of 16 `String` strings"
