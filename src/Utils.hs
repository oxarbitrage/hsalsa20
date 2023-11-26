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

module Utils
    (
        littleendian, extractBytes4, extractBytes8, displayBytes4, displayBytes8,
        Utils.reduce, reduceDisplay, reduceKeelung,
        aument, aumentDisplay, aumentKeelung,
        modMatrix, numberListToStringList, transpose, modMatrixDisplay, modMatrixKeelung,
        eitherListToNumberList, eitherListToStringList,
    )
where

import Data.Bits
import Data.Word
import Data.List.Split
import Text.Printf
import Data.Either (fromLeft, fromRight)

import Keelung hiding (input, eq)

import Operators()

-- | Encode a vector as a word using little-endian byte order.
{-@ littleendian ::  { i:[_] | (len i) == 4 } -> _ @-}
littleendian :: [Word32] -> Word32
littleendian bytes = sum [byte `Data.Bits.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

-- |The `littleendian` expression as a string.
{-@ littleendianDisplay ::  { i:[_] | (len i) == 4 } -> _ @-}
littleendianDisplay :: [String] -> String
littleendianDisplay [b0, b1, b2, b3] = printf "%s + 2^8 * %s + 2^16 * %s + 2^24 * %s" b0 b1 b2 b3
littleendianDisplay _ = ""

-- | Encode a vector as a word using little-endian byte order using Keelung expressions.
littleendianKeelung :: [UInt 32] -> UInt 32
littleendianKeelung bytes = sum [byte `Keelung.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

{-| Extract 4 bytes from a Word32.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ extractBytes4 ::  _ -> { o:[_] | (len o) == 4 } @-}
extractBytes4 :: Word32 -> [Word32]
extractBytes4 w = [ Data.Bits.shiftR w (8 * 0) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 1) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 2) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 3) Data.Bits..&. 0xff]

{-| Extract 8 bytes from a Word32.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ extractBytes8 ::  _ -> { o:[_] | (len o) == 8 } @-}
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
{-@ displayBytes4 ::  _ -> { o:[_] | (len o) == 4 } @-}
displayBytes4 :: String -> [String]
displayBytes4 w =
    [ printf "%s & 255" w,  printf "%s & 255" w, printf "%s & 255" w, printf "%s & 255" w]

{-| Display a 8 bytes from a string as a list of strings.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ displayBytes8 ::  _ -> { o:[_] | (len o) == 8 } @-}
displayBytes8 :: String -> [String]
displayBytes8 w =
    [ 
        printf "%s & 255" w,  printf "%s & 255" w, printf "%s & 255" w, printf "%s & 255" w,
        printf "%d >>> %s & 255" ((8 * 4)::Int) w, printf "%d >>> %s & 255" ((8 * 5)::Int) w,
        printf "%d >>> %s & 255" ((8 * 6)::Int) w, printf "%d >>> %s & 255" ((8 * 7)::Int) w]

-- | Extract a specified number of bytes from a Word32 using Keelung expressions.
extractBytesKeelung :: Int -> UInt 32 -> [UInt 32]
extractBytesKeelung numBytes w =
    [ Keelung.shiftR w (8 * i) Keelung..&. 0xff | i <- [0 .. numBytes - 1] ]

{-| Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ reduce ::  { i:[_] | (len i) == 64 } -> { o:[_] | (len o) == 16 } @-}
reduce :: [Word32] -> [Word32]
reduce input = map littleendian [
    [input!!0, input!!1, input!!2, input!!3],
    [input!!4, input!!5, input!!6, input!!7],
    [input!!8, input!!9, input!!10, input!!11],
    [input!!12, input!!13, input!!14, input!!15],
    [input!!16, input!!17, input!!18, input!!19],
    [input!!20, input!!21, input!!22, input!!23],
    [input!!24, input!!25, input!!26, input!!27],
    [input!!28, input!!29, input!!30, input!!31],
    [input!!32, input!!33, input!!34, input!!35],
    [input!!36, input!!37, input!!38, input!!39],
    [input!!40, input!!41, input!!42, input!!43],
    [input!!44, input!!45, input!!46, input!!47],
    [input!!48, input!!49, input!!50, input!!51],
    [input!!52, input!!53, input!!54, input!!55],
    [input!!56, input!!57, input!!58, input!!59],
    [input!!60, input!!61, input!!62, input!!63]]

{-| Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianDisplay` encoding.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ reduceDisplay ::  { i:[_] | (len i) == 64 } -> { o:[_] | (len o) == 16 } @-}
reduceDisplay :: [String] -> [String]
reduceDisplay input = map littleendianDisplay [
    [input!!0, input!!1, input!!2, input!!3],
    [input!!4, input!!5, input!!6, input!!7],
    [input!!8, input!!9, input!!10, input!!11],
    [input!!12, input!!13, input!!14, input!!15],
    [input!!16, input!!17, input!!18, input!!19],
    [input!!20, input!!21, input!!22, input!!23],
    [input!!24, input!!25, input!!26, input!!27],
    [input!!28, input!!29, input!!30, input!!31],
    [input!!32, input!!33, input!!34, input!!35],
    [input!!36, input!!37, input!!38, input!!39],
    [input!!40, input!!41, input!!42, input!!43],
    [input!!44, input!!45, input!!46, input!!47],
    [input!!48, input!!49, input!!50, input!!51],
    [input!!52, input!!53, input!!54, input!!55],
    [input!!56, input!!57, input!!58, input!!59],
    [input!!60, input!!61, input!!62, input!!63]]

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianKeelung` encoding.
reduceKeelung :: [UInt 32] -> [UInt 32]
reduceKeelung input = map littleendianKeelung $ chunksOf 4 input

{-| Aument a matrix of 16 elements to one of 64 elements by using `extractBytes`.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ aument ::  { i:[Word32] | (len i) == 16 } -> { o:[Word32] | (len o) == 64 } @-}
aument :: [Word32] -> [Word32]
aument input = [
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

{-| Aument a matrix of 16 elements to one of 64 elements by using `displayBytes`.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ aumentDisplay ::  { i:[String] | (len i) == 16 } -> { o:[String] | (len o) == 64 } @-}
aumentDisplay :: [String] -> [String]
aumentDisplay input = [
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

-- |Aument a matrix of 16 elements to one of 64 elements by using `extractBytesKeelung`.
aumentKeelung :: [UInt 32] -> [UInt 32]
aumentKeelung = concatMap $ extractBytesKeelung 4

{-| Given two matrices, do modulo addition on each of the elements.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ modMatrix ::  { i:[_] | (len i) == 16 } -> { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix a b = [
    a!!0 + b!!0, a!!1 + b!!1, a!!2 + b!!2, a!!3 + b!!3,
    a!!4 + b!!4, a!!5 + b!!5, a!!6 + b!!6, a!!7 + b!!7,
    a!!8 + b!!8, a!!9 + b!!9, a!!10 + b!!10, a!!11 + b!!11,
    a!!12 + b!!12, a!!13 + b!!13, a!!14 + b!!14, a!!15 + b!!15]

{-| Given two matrices, display the modulo addition on each of the elements.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ modMatrixDisplay ::  { i:[_] | (len i) == 16 } -> { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 16 } @-}
modMatrixDisplay :: [String] -> [String] -> [String]
modMatrixDisplay a b = 
    [
        a!!0 ++ " + " ++ b!!0, a!!1 ++ " + " ++ b!!1, a!!2 ++ " + " ++ b!!2, a!!3 ++ " + " ++ b!!3,
        a!!4 ++ " + " ++ b!!4, a!!5 ++ " + " ++ b!!5, a!!6 ++ " + " ++ b!!6, a!!7 ++ " + " ++ b!!7,
        a!!8 ++ " + " ++ b!!8, a!!9 ++ " + " ++ b!!9, a!!10 ++ " + " ++ b!!10, a!!11 ++ " + " ++ b!!11,
        a!!12 ++ " + " ++ b!!12, a!!13 ++ " + " ++ b!!13, a!!14 ++ " + " ++ b!!14, a!!15 ++ " + " ++ b!!15]

-- |Given two matrices, do modulo addition on each of the elements using Keelung types.
modMatrixKeelung :: [UInt 32] -> [UInt 32] -> [UInt 32]
modMatrixKeelung = zipWith (+)

-- |Transpose a 4x4 matrix type.
{-@ transpose :: {i:[_] | len i == 16} -> {o:[_] | len o == 16} @-}
transpose :: [a] -> [a]
transpose [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] = 
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transpose _ = error "input to `transpose` must be a list of 16 objects"

-- |Convert a list of numbers to a list of strings. This is always possible. 
numberListToStringList :: [Word32] -> [String]
numberListToStringList = map (\x -> printf "%d" x)

-- |Convert a list of `Either` type to an list of numbers.
{-@ eitherListToNumberList :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 }  @-}
eitherListToNumberList :: [Either Word32 String] -> [Word32]
eitherListToNumberList input
    | length input == 4 = map (fromLeft 0) input
    | otherwise = error "input to `eitherListToNumberList` must be a list of 4 `Either` objects"

-- |Convert a list of `Either` type to a list of strings.
{-@ eitherListToStringList :: { i:[_] | (len i) == 4 } -> { o:[_] | (len o) == 4 }  @-}
eitherListToStringList :: [Either Word32 String] -> [String]
eitherListToStringList = map (fromRight "0")
