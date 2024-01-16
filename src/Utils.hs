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
        littleendian, extractBytes4, extractBytes8, displayBytes4, displayBytes8,
        Utils.reduce, reduceDisplay, reduceKeelung,
        aument, aumentDisplay, aumentKeelung,
        modMatrix, numberListToStringList, transposev1, transposev2, transposev3, modMatrixDisplay, modMatrixKeelung,
        eitherListToNumberList, eitherListToStringList,
        elts,
    )
where

import Data.Bits
import Data.Word
import Text.Printf
import Data.Either (fromLeft, fromRight)

import Keelung hiding (input, eq)

import Data.Set as Set

import Operators()

-- | Encode a vector as a word using little-endian byte order.
{-@ littleendian :: { i:[Word32] | (len i) == 4 } -> Word32 @-}
littleendian :: [Word32] -> Word32
littleendian bytes = sum [byte `Data.Bits.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

-- |The `littleendian` expression as a string.
{-@ littleendianDisplay :: { i:[String] | (len i) == 4 } -> String @-}
littleendianDisplay :: [String] -> String
littleendianDisplay [b0, b1, b2, b3] = printf "%s + 2^8 * %s + 2^16 * %s + 2^24 * %s" b0 b1 b2 b3
littleendianDisplay _ = ""

-- | Encode a vector as a word using little-endian byte order using Keelung expressions.
{-@ littleendianKeelung :: { i:[_] | (len i) == 4 } -> _ @-}
littleendianKeelung :: [UInt 32] -> UInt 32
littleendianKeelung bytes = sum [byte `Keelung.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

{-| Extract 4 bytes from a Word32.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ extractBytes4 :: Word32 -> { o:[Word32] | (len o) == 4 } @-}
extractBytes4 :: Word32 -> [Word32]
extractBytes4 w = [ Data.Bits.shiftR w (8 * 0) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 1) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 2) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 3) Data.Bits..&. 0xff]

{-| Extract 8 bytes from a Word32.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ extractBytes8 :: Word32 -> { o:[Word32] | (len o) == 8 } @-}
extractBytes8 :: Word32 -> [Word32]
extractBytes8 w = [ Data.Bits.shiftR w (8 * 0) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 1) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 2) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 3) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 4) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 5) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 6) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 7) Data.Bits..&. 0xff]

{-| Display a 4 bytes from a string as a list of strings.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ displayBytes4 :: String -> { o:[String] | (len o) == 4 } @-}
displayBytes4 :: String -> [String]
displayBytes4 w =
    [ printf "%s & 255" w,  printf "%s & 255" w, printf "%s & 255" w, printf "%s & 255" w]

{-| Display a 8 bytes from a string as a list of strings.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ displayBytes8 :: String -> { o:[String] | (len o) == 8 } @-}
displayBytes8 :: String -> [String]
displayBytes8 w =
    [ 
        printf "%s & 255" w,  printf "%s & 255" w, printf "%s & 255" w, printf "%s & 255" w,
        printf "%d >>> %s & 255" ((8 * 4)::Int) w, printf "%d >>> %s & 255" ((8 * 5)::Int) w,
        printf "%d >>> %s & 255" ((8 * 6)::Int) w, printf "%d >>> %s & 255" ((8 * 7)::Int) w]

-- | Extract a specified number of bytes from a Word32 using Keelung expressions.
{-@ extractBytes4Keelung :: _ -> { o:[_] | (len o) == 4 } @-}
extractBytes4Keelung :: UInt 32 -> [UInt 32]
extractBytes4Keelung w = [ Keelung.shiftR w (8 * 0) Keelung..&. 0xff,
        Keelung.shiftR w (8 * 1) Keelung..&. 0xff,
        Keelung.shiftR w (8 * 2) Keelung..&. 0xff,
        Keelung.shiftR w (8 * 3) Keelung..&. 0xff]

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

-- | Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
{-@ reduce :: { i:[Word32] | (len i) == 64 } -> { o:[Word32] | (len o) == 16 } @-}
reduce :: [Word32] -> [Word32]
reduce input = Prelude.map littleendian (chunksof4v1 input)

-- | Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianDisplay` encoding.
{-@ reduceDisplay :: { i:[String] | (len i) == 64 } -> { o:[String] | (len o) == 16 } @-}
reduceDisplay :: [String] -> [String]
reduceDisplay input = Prelude.map littleendianDisplay (chunksof4v2 input)

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianKeelung` encoding.
{-@ reduceKeelung :: { i:[_] | (len i) == 64 } -> { o:[_] | (len o) == 16 } @-}
reduceKeelung :: [UInt 32] -> [UInt 32]
reduceKeelung input = Prelude.map littleendianKeelung (chunksof4v3 input)

-- |Internal general type aument function used in `aument`, `aumentDisplay` and `aumentKeelung`.
{-@ aumentv1 :: { i:[Word32] | (len i) == 16 } -> { o:[Word32] | (len o) == 64 } @-}
aumentv1 :: [Word32] -> [Word32]
aumentv1 input = [
    extractBytes4 (input!!0)!!0, extractBytes4 (input!!0)!!1, extractBytes4 (input!!0)!!2, extractBytes4 (input!!0)!!3,
    extractBytes4 (input!!1)!!0, extractBytes4 (input!!1)!!1, extractBytes4 (input!!1)!!2, extractBytes4 (input!!1)!!3,
    extractBytes4 (input!!2)!!0, extractBytes4 (input!!2)!!1, extractBytes4 (input!!2)!!2, extractBytes4 (input!!2)!!3,
    extractBytes4 (input!!3)!!0, extractBytes4 (input!!3)!!1, extractBytes4 (input!!3)!!2, extractBytes4 (input!!3)!!3,
    extractBytes4 (input!!4)!!0, extractBytes4 (input!!4)!!1, extractBytes4 (input!!4)!!2, extractBytes4 (input!!4)!!3,
    extractBytes4 (input!!5)!!0, extractBytes4 (input!!5)!!1, extractBytes4 (input!!5)!!2, extractBytes4 (input!!5)!!3,
    extractBytes4 (input!!6)!!0, extractBytes4 (input!!6)!!1, extractBytes4 (input!!6)!!2, extractBytes4 (input!!6)!!3,
    extractBytes4 (input!!7)!!0, extractBytes4 (input!!7)!!1, extractBytes4 (input!!7)!!2, extractBytes4 (input!!7)!!3,
    extractBytes4 (input!!8)!!0, extractBytes4 (input!!8)!!1, extractBytes4 (input!!8)!!2, extractBytes4 (input!!8)!!3,
    extractBytes4 (input!!9)!!0, extractBytes4 (input!!9)!!1, extractBytes4 (input!!9)!!2, extractBytes4 (input!!9)!!3,
    extractBytes4 (input!!10)!!0, extractBytes4 (input!!10)!!1, extractBytes4 (input!!10)!!2, extractBytes4 (input!!10)!!3,
    extractBytes4 (input!!11)!!0, extractBytes4 (input!!11)!!1, extractBytes4 (input!!11)!!2, extractBytes4 (input!!11)!!3,
    extractBytes4 (input!!12)!!0, extractBytes4 (input!!12)!!1, extractBytes4 (input!!12)!!2, extractBytes4 (input!!12)!!3,
    extractBytes4 (input!!13)!!0, extractBytes4 (input!!13)!!1, extractBytes4 (input!!13)!!2, extractBytes4 (input!!13)!!3,
    extractBytes4 (input!!14)!!0, extractBytes4 (input!!14)!!1, extractBytes4 (input!!14)!!2, extractBytes4 (input!!14)!!3,
    extractBytes4 (input!!15)!!0, extractBytes4 (input!!15)!!1, extractBytes4 (input!!15)!!2, extractBytes4 (input!!15)!!3]

{-@ aumentv2 :: { i:[String] | (len i) == 16 } -> { o:[String] | (len o) == 64 } @-}
aumentv2 :: [String] -> [String]
aumentv2 input = [
    displayBytes4 (input!!0)!!0, displayBytes4 (input!!0)!!1, displayBytes4 (input!!0)!!2, displayBytes4 (input!!0)!!3,
    displayBytes4 (input!!1)!!0, displayBytes4 (input!!1)!!1, displayBytes4 (input!!1)!!2, displayBytes4 (input!!1)!!3,
    displayBytes4 (input!!2)!!0, displayBytes4 (input!!2)!!1, displayBytes4 (input!!2)!!2, displayBytes4 (input!!2)!!3,
    displayBytes4 (input!!3)!!0, displayBytes4 (input!!3)!!1, displayBytes4 (input!!3)!!2, displayBytes4 (input!!3)!!3,
    displayBytes4 (input!!4)!!0, displayBytes4 (input!!4)!!1, displayBytes4 (input!!4)!!2, displayBytes4 (input!!4)!!3,
    displayBytes4 (input!!5)!!0, displayBytes4 (input!!5)!!1, displayBytes4 (input!!5)!!2, displayBytes4 (input!!5)!!3,
    displayBytes4 (input!!6)!!0, displayBytes4 (input!!6)!!1, displayBytes4 (input!!6)!!2, displayBytes4 (input!!6)!!3,
    displayBytes4 (input!!7)!!0, displayBytes4 (input!!7)!!1, displayBytes4 (input!!7)!!2, displayBytes4 (input!!7)!!3,
    displayBytes4 (input!!8)!!0, displayBytes4 (input!!8)!!1, displayBytes4 (input!!8)!!2, displayBytes4 (input!!8)!!3,
    displayBytes4 (input!!9)!!0, displayBytes4 (input!!9)!!1, displayBytes4 (input!!9)!!2, displayBytes4 (input!!9)!!3,
    displayBytes4 (input!!10)!!0, displayBytes4 (input!!10)!!1, displayBytes4 (input!!10)!!2, displayBytes4 (input!!10)!!3,
    displayBytes4 (input!!11)!!0, displayBytes4 (input!!11)!!1, displayBytes4 (input!!11)!!2, displayBytes4 (input!!11)!!3,
    displayBytes4 (input!!12)!!0, displayBytes4 (input!!12)!!1, displayBytes4 (input!!12)!!2, displayBytes4 (input!!12)!!3,
    displayBytes4 (input!!13)!!0, displayBytes4 (input!!13)!!1, displayBytes4 (input!!13)!!2, displayBytes4 (input!!13)!!3,
    displayBytes4 (input!!14)!!0, displayBytes4 (input!!14)!!1, displayBytes4 (input!!14)!!2, displayBytes4 (input!!14)!!3,
    displayBytes4 (input!!15)!!0, displayBytes4 (input!!15)!!1, displayBytes4 (input!!15)!!2, displayBytes4 (input!!15)!!3]

{-@ aumentv3 :: { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 64 } @-}
aumentv3 :: [UInt 32] -> [UInt 32]
aumentv3 input = [
    extractBytes4Keelung (input!!0)!!0, extractBytes4Keelung (input!!0)!!1, extractBytes4Keelung (input!!0)!!2, extractBytes4Keelung (input!!0)!!3,
    extractBytes4Keelung (input!!1)!!0, extractBytes4Keelung (input!!1)!!1, extractBytes4Keelung (input!!1)!!2, extractBytes4Keelung (input!!1)!!3,
    extractBytes4Keelung (input!!2)!!0, extractBytes4Keelung (input!!2)!!1, extractBytes4Keelung (input!!2)!!2, extractBytes4Keelung (input!!2)!!3,
    extractBytes4Keelung (input!!3)!!0, extractBytes4Keelung (input!!3)!!1, extractBytes4Keelung (input!!3)!!2, extractBytes4Keelung (input!!3)!!3,
    extractBytes4Keelung (input!!4)!!0, extractBytes4Keelung (input!!4)!!1, extractBytes4Keelung (input!!4)!!2, extractBytes4Keelung (input!!4)!!3,
    extractBytes4Keelung (input!!5)!!0, extractBytes4Keelung (input!!5)!!1, extractBytes4Keelung (input!!5)!!2, extractBytes4Keelung (input!!5)!!3,
    extractBytes4Keelung (input!!6)!!0, extractBytes4Keelung (input!!6)!!1, extractBytes4Keelung (input!!6)!!2, extractBytes4Keelung (input!!6)!!3,
    extractBytes4Keelung (input!!7)!!0, extractBytes4Keelung (input!!7)!!1, extractBytes4Keelung (input!!7)!!2, extractBytes4Keelung (input!!7)!!3,
    extractBytes4Keelung (input!!8)!!0, extractBytes4Keelung (input!!8)!!1, extractBytes4Keelung (input!!8)!!2, extractBytes4Keelung (input!!8)!!3,
    extractBytes4Keelung (input!!9)!!0, extractBytes4Keelung (input!!9)!!1, extractBytes4Keelung (input!!9)!!2, extractBytes4Keelung (input!!9)!!3,
    extractBytes4Keelung (input!!10)!!0, extractBytes4Keelung (input!!10)!!1, extractBytes4Keelung (input!!10)!!2, extractBytes4Keelung (input!!10)!!3,
    extractBytes4Keelung (input!!11)!!0, extractBytes4Keelung (input!!11)!!1, extractBytes4Keelung (input!!11)!!2, extractBytes4Keelung (input!!11)!!3,
    extractBytes4Keelung (input!!12)!!0, extractBytes4Keelung (input!!12)!!1, extractBytes4Keelung (input!!12)!!2, extractBytes4Keelung (input!!12)!!3,
    extractBytes4Keelung (input!!13)!!0, extractBytes4Keelung (input!!13)!!1, extractBytes4Keelung (input!!13)!!2, extractBytes4Keelung (input!!13)!!3,
    extractBytes4Keelung (input!!14)!!0, extractBytes4Keelung (input!!14)!!1, extractBytes4Keelung (input!!14)!!2, extractBytes4Keelung (input!!14)!!3,
    extractBytes4Keelung (input!!15)!!0, extractBytes4Keelung (input!!15)!!1, extractBytes4Keelung (input!!15)!!2, extractBytes4Keelung (input!!15)!!3]

-- | Aument a matrix of 16 elements to one of 64 elements by using `extractBytes`.
{-@ aument :: { i:[Word32] | (len i) == 16 } -> { o:[Word32] | (len o) == 64 } @-}
aument :: [Word32] -> [Word32]
aument = aumentv1

-- | Aument a matrix of 16 elements to one of 64 elements by using `displayBytes`.
{-@ aumentDisplay :: { i:[String] | (len i) == 16 } -> { o:[String] | (len o) == 64 } @-}
aumentDisplay :: [String] -> [String]
aumentDisplay = aumentv2

-- |Aument a matrix of 16 elements to one of 64 elements by using `extractBytesKeelung`.
{-@ aumentKeelung ::  { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 64 } @-}
{-@ ignore aumentKeelung @-}
aumentKeelung :: [UInt 32] -> [UInt 32]
aumentKeelung = aumentv3

-- |Internal version of `zipWith` that works with refinements.
zipWithv1 :: (Word32 -> Word32 -> Word32) -> [Word32] -> [Word32] -> [Word32]
zipWithv1 _ [] _ = []
zipWithv1 _ _ [] = []
zipWithv1 f (x:xs) (y:ys) = f x y : zipWithv1 f xs ys

-- |Internal version of `zipWith` that works with refinements.
zipWithv2 :: (String -> String -> String) -> [String] -> [String] -> [String]
zipWithv2 _ [] _ = []
zipWithv2 _ _ [] = []
zipWithv2 f (x:xs) (y:ys) = f x y : zipWithv2 f xs ys

-- |Internal version of `zipWith` that works with refinements.
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

-- Stuff needed for liquidhaskell refinement types

{-@ measure elts @-}
elts        :: (Ord a) => [a] -> Set a
elts []     = empty
elts (x:xs) = singleton x `union` elts xs
