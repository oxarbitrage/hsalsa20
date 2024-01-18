{-|
Module      : Utils
Description : General utilities for Salsa20 cipher implementation.
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module provides general utility functions used in the creation of the Salsa20 cipher.

-}
{-# LANGUAGE DataKinds #-}
{-@ LIQUID "--prune-unsorted" @-}

module Utils
    (
        modMatrix, numberListToStringList, transposev1, transposev2, transposev3, modMatrixDisplay, modMatrixKeelung,
        eitherListToNumberList, eitherListToStringList,
        elts, concat64,
        chunksof4v1, chunksof4v2, chunksof4v3,
    )
where

import Data.Word
import Text.Printf
import Data.Either (fromLeft, fromRight)

import Keelung hiding (input, eq)

import Data.Set as Set

import Operators()

-- | Get chunks of 4 elements from a list of 64 elements.
{-@ chunksof4v1 :: { i:[Word32] | (len i) == 64 } -> { o:[{ inner_o:[Word32] | (len inner_o) == 4}] | (len o) == 16 } @-}
chunksof4v1 :: [Word32] -> [[Word32]]
chunksof4v1 input
    | length input == 64 = [
        Prelude.take 4 input,
        Prelude.take 4 (Prelude.drop 4 input),
        Prelude.take 4 (Prelude.drop 8 input),
        Prelude.take 4 (Prelude.drop 12 input),
        Prelude.take 4 (Prelude.drop 16 input),
        Prelude.take 4 (Prelude.drop 20 input),
        Prelude.take 4 (Prelude.drop 24 input),
        Prelude.take 4 (Prelude.drop 28 input),
        Prelude.take 4 (Prelude.drop 32 input),
        Prelude.take 4 (Prelude.drop 36 input),
        Prelude.take 4 (Prelude.drop 40 input),
        Prelude.take 4 (Prelude.drop 44 input),
        Prelude.take 4 (Prelude.drop 48 input),
        Prelude.take 4 (Prelude.drop 52 input),
        Prelude.take 4 (Prelude.drop 56 input),
        Prelude.take 4 (Prelude.drop 60 input)]
    | otherwise = error "input to `chunksof4` must be a list of 64 elements"

-- | Get chunks of 4 elements from a list of 64 elements.
{-@ chunksof4v2 :: { i:[String] | (len i) == 64 } -> { o:[{ inner_o:[String] | (len inner_o) == 4}] | (len o) == 16 } @-}
chunksof4v2 :: [String] -> [[String]]
chunksof4v2 input
    | length input == 64 = [
        Prelude.take 4 input,
        Prelude.take 4 (Prelude.drop 4 input),
        Prelude.take 4 (Prelude.drop 8 input),
        Prelude.take 4 (Prelude.drop 12 input),
        Prelude.take 4 (Prelude.drop 16 input),
        Prelude.take 4 (Prelude.drop 20 input),
        Prelude.take 4 (Prelude.drop 24 input),
        Prelude.take 4 (Prelude.drop 28 input),
        Prelude.take 4 (Prelude.drop 32 input),
        Prelude.take 4 (Prelude.drop 36 input),
        Prelude.take 4 (Prelude.drop 40 input),
        Prelude.take 4 (Prelude.drop 44 input),
        Prelude.take 4 (Prelude.drop 48 input),
        Prelude.take 4 (Prelude.drop 52 input),
        Prelude.take 4 (Prelude.drop 56 input),
        Prelude.take 4 (Prelude.drop 60 input)]
    | otherwise = error "input to `chunksof4` must be a list of 64 elements"

-- | Get chunks of 4 elements from a list of 64 elements.
{-@ chunksof4v3 :: { i:[_] | (len i) == 64 } -> { o:[{ inner_o:[_] | (len inner_o) == 4}] | (len o) == 16 } @-}
chunksof4v3 :: [UInt 32] -> [[UInt 32]]
chunksof4v3 input
    | length input == 64 = [
        Prelude.take 4 input,
        Prelude.take 4 (Prelude.drop 4 input),
        Prelude.take 4 (Prelude.drop 8 input),
        Prelude.take 4 (Prelude.drop 12 input),
        Prelude.take 4 (Prelude.drop 16 input),
        Prelude.take 4 (Prelude.drop 20 input),
        Prelude.take 4 (Prelude.drop 24 input),
        Prelude.take 4 (Prelude.drop 28 input),
        Prelude.take 4 (Prelude.drop 32 input),
        Prelude.take 4 (Prelude.drop 36 input),
        Prelude.take 4 (Prelude.drop 40 input),
        Prelude.take 4 (Prelude.drop 44 input),
        Prelude.take 4 (Prelude.drop 48 input),
        Prelude.take 4 (Prelude.drop 52 input),
        Prelude.take 4 (Prelude.drop 56 input),
        Prelude.take 4 (Prelude.drop 60 input)]
    | otherwise = error "input to `chunksof4` must be a list of 64 elements"

-- |Monomorphic `Word32` version of `zipWith` that works with refinements.
zipWithv1 :: (Word32 -> Word32 -> Word32) -> [Word32] -> [Word32] -> [Word32]
zipWithv1 _ [] _ = []
zipWithv1 _ _ [] = []
zipWithv1 f (x:xs) (y:ys) = f x y : zipWithv1 f xs ys

-- |Monomorphic `String` version of `zipWith` that works with refinements.
zipWithv2 :: (String -> String -> String) -> [String] -> [String] -> [String]
zipWithv2 _ [] _ = []
zipWithv2 _ _ [] = []
zipWithv2 f (x:xs) (y:ys) = f x y : zipWithv2 f xs ys

-- |Monomorphic `UInt 32` version of `zipWith` that works with refinements.
zipWithv3 :: (UInt 32 -> UInt 32 -> UInt 32) -> [UInt 32] -> [UInt 32] -> [UInt 32]
zipWithv3 _ [] _ = []
zipWithv3 _ _ [] = []
zipWithv3 f (x:xs) (y:ys) = f x y : zipWithv3 f xs ys

-- |Given two matrices, do modulo addition on each of the elements.
{-@ modMatrix :: { i:[Word32] | (len i) == 16 } -> { i:[Word32] | (len i) == 16 } -> { o:[Word32] | (len o) == 16 } @-}
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWithv1 (+)

-- |Given two matrices, display the modulo addition on each of the elements.
{-@ modMatrixDisplay :: { i:[String] | (len i) == 16 } -> { i:[String] | (len i) == 16 } -> { o:[String] | (len o) == 16 } @-}
modMatrixDisplay :: [String] -> [String] -> [String]
modMatrixDisplay = zipWithv2 (printf "%s + %s")

-- |Given two matrices, do modulo addition on each of the elements using Keelung types.
{-@ modMatrixKeelung :: { a:[_] | (len a) == 16 } -> { b:[_] | (len b) == 16 } -> { o:[_] | (len o) == 16 } @-}
modMatrixKeelung :: [UInt 32] -> [UInt 32] -> [UInt 32]
modMatrixKeelung = zipWithv3 Keelung.AddU

-- |Transpose a 4x4 matrix type.
{-@ transposev1 :: {i:[Word32] | len i == 16} -> {o:[Word32] | len o == 16 && (elts i == elts o) } @-}
transposev1 :: [Word32] -> [Word32]
transposev1 [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] =
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transposev1 _ = error "input to `transpose` must be a list of 16 objects"

-- |Transpose a 4x4 matrix type.
{-@ transposev2 :: {i:[String] | len i == 16} -> {o:[String] | len o == 16 && (elts i == elts o) } @-}
transposev2 :: [String] -> [String]
transposev2 [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] =
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transposev2 _ = error "input to `transpose` must be a list of 16 objects"

-- |Transpose a 4x4 matrix type.
{-@ transposev3 :: {i:[_] | len i == 16} -> {o:[_] | len o == 16 && (elts i == elts o) } @-}
transposev3 :: [UInt 32] -> [UInt 32]
transposev3 [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] =
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transposev3 _ = error "input to `transpose` must be a list of 16 objects"

-- |Convert a list of numbers to a list of strings. This is always possible. 
{-@ numberListToStringList :: { i:[Word32] | (len i) == 4 } -> { o:[String] | (len o) == 4 }  @-}
numberListToStringList :: [Word32] -> [String]
numberListToStringList = Prelude.map (printf "%d")

-- |Convert a list of `Either` type to an list of numbers.
{-@ eitherListToNumberList :: { i:[Either Word32 String] | (len i) == 4 } -> { o:[Word32] | (len o) == 4 }  @-}
eitherListToNumberList :: [Either Word32 String] -> [Word32]
eitherListToNumberList input
    | length input == 4 = Prelude.map (fromLeft 0) input
    | otherwise = error "input to `eitherListToNumberList` must be a list of 4 `Either` objects"

-- |Convert a list of `Either` type to a list of strings.
{-@ eitherListToStringList :: { i:[Either Word32 String] | (len i) == 4 } -> { o:[String] | (len o) == 4 }  @-}
eitherListToStringList :: [Either Word32 String] -> [String]
eitherListToStringList = Prelude.map (fromRight "0")

-- |Track elements inside a list.
{-@ measure elts @-}
elts        :: (Ord a) => [a] -> Set a
elts []     = empty
elts (x:xs) = singleton x `union` elts xs

{-
A concat version where all the lenghts are known and assumed:
- The lenght of ezch chunk in the inner list is 4.
- The lenght of the outer list is 16.
- The lenght of the resulting list is 64.
-}
{-@ assume concat64 :: {outer:[{inner:[a] | len inner == 4}] | len outer == 16} -> {output:[a] | len output == 64} @-}
concat64 :: [[a]] -> [a]
concat64 = concat
