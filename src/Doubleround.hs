{-|
Module      : Doubleround
Description : Doubleround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We define the doubleround function as the composition of rowround and columnround.

In addition we define `doubleroundR`, which is the `doubleround` function applied R times,
as specified in the salsa20 spec.
-}
module Doubleround
    (
        doubleroundCompute, doubleroundDisplay, doubleroundEquations,
        doubleroundRCompute, doubleroundRDisplay, doubleroundREquations,
    )
where

import Data.Word
import Text.Printf

import Rowround
import Columnround

-- |The doubleround expression computed.
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = rowroundCompute $ columnroundCompute input
doubleroundCompute _ = error "input to `doubleroundCompute` must be a list of 16 `Word32` numbers"

-- |The doubleround expression as a string.
doubleroundDisplay :: [String] -> [String]
doubleroundDisplay input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = rowroundDisplay $ columnroundDisplay input
doubleroundDisplay _ = error "input to `doubleroundDisplay` must be a list of 16 `String` strings"

-- |The doubleround expression as a list of equations.
doubleroundEquations :: [String] -> [String]
doubleroundEquations input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
    [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (doubleroundDisplay input)]
doubleroundEquations _ = error "input to `doubleroundEquations` must be a list of 16 `String` strings"

-- |The doubleroundR expression computed.
doubleroundRCompute :: [Word32] -> Int -> [Word32]
doubleroundRCompute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] 0 = input
doubleroundRCompute input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] r =
    doubleroundRCompute (doubleroundCompute input) (r - 1)
doubleroundRCompute _ _ =
    error "arguments of `doubleroundRCompute` must be a list of 16 `Word32` numbers and a number `Int` of rounds"

-- |The doubleroundR expression as a string.
doubleroundRDisplay :: [String] -> Int -> [String]
doubleroundRDisplay input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] 0 = input
doubleroundRDisplay input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] r =
    doubleroundRDisplay (doubleroundDisplay input) (r - 1)
doubleroundRDisplay _ _ =
    error "arguments of `doubleroundRDisplay` must be a list of 16 `String` strings and a number `Int` of rounds"

-- |The doubleroundR expression as a list of equations.
doubleroundREquations :: [String] -> Int -> [String]
doubleroundREquations input@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] r =
    [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (doubleroundRDisplay input r)]
doubleroundREquations _ _ =
    error "arguments of `doubleroundREquations` must be a list of 16 `String` strings and a number `Int` of rounds"
