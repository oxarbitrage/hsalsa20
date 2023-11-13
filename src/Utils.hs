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
        littleendian, extractBytes, displayBytes, Utils.reduce, reduceDisplay, reduceKeelung,
        aument, aumentDisplay, aumentKeelung,
        modMatrix, numberListToStringList, transpose, modMatrixDisplay, modMatrixKeelung,
        numberListToEitherList, stringListToEitherList, eitherListToNumberList, eitherListToStringList,
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
littleendian :: [Word32] -> Word32
littleendian bytes = sum [byte `Data.Bits.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

-- |The `littleendian` expression as a string.
littleendianDisplay :: [String] -> String
littleendianDisplay [b0, b1, b2, b3] = printf "%s + 2^8 * %s + 2^16 * %s + 2^24 * %s" b0 b1 b2 b3
littleendianDisplay _ = ""

-- | Encode a vector as a word using little-endian byte order using Keelung expressions.
littleendianKeelung :: [UInt 32] -> UInt 32
littleendianKeelung bytes = sum [byte `Keelung.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

-- | Extract a specified number of bytes from a Word32.
extractBytes :: Int -> Word32 -> [Word32]
extractBytes numBytes w =
    [ Data.Bits.shiftR w (8 * i) Data.Bits..&. 0xff | i <- [0 .. numBytes - 1] ]

-- | Display a specified number of bytes from a string as a list of strings.
displayBytes :: Int -> String -> [String]
displayBytes numBytes w =
    [ printf "%s & 255" w | _ <- [1 .. numBytes] ] ++
    [ printf "%d >>> %s & 255" (8 * i) w | i <- [numBytes .. 7] ]

-- | Extract a specified number of bytes from a Word32 using Keelung expressions.
extractBytesKeelung :: Int -> UInt 32 -> [UInt 32]
extractBytesKeelung numBytes w =
    [ Keelung.shiftR w (8 * i) Keelung..&. 0xff | i <- [0 .. numBytes - 1] ]

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
reduce :: [Word32] -> [Word32]
reduce input = map littleendian $ chunksOf 4 input

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianDisplay` encoding.
reduceDisplay :: [String] -> [String]
reduceDisplay input = map littleendianDisplay $ chunksOf 4 input

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianKeelung` encoding.
reduceKeelung :: [UInt 32] -> [UInt 32]
reduceKeelung input = map littleendianKeelung $ chunksOf 4 input

-- |Aument a matrix of 16 elements to one of 64 elements by using `extractBytes`.
aument :: [Word32] -> [Word32]
aument = concatMap $ extractBytes 4

-- |Aument a matrix of 16 elements to one of 64 elements by using `displayBytes`.
aumentDisplay :: [String] -> [String]
aumentDisplay = concatMap $ displayBytes 4

-- |Aument a matrix of 16 elements to one of 64 elements by using `extractBytesKeelung`.
aumentKeelung :: [UInt 32] -> [UInt 32]
aumentKeelung = concatMap $ extractBytesKeelung 4

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWith (+)

-- |Given two matrices, display the modulo addition on each of the elements.
modMatrixDisplay :: [String] -> [String] -> [String]
modMatrixDisplay = zipWith displayMod

-- |Given two matrices, do modulo addition on each of the elements using Keelung types.
modMatrixKeelung :: [UInt 32] -> [UInt 32] -> [UInt 32]
modMatrixKeelung = zipWith (+)

-- Append modulo addition symbol and a string to a given string.
displayMod :: String -> String -> String
displayMod a b = a ++ " + " ++ b

-- |Transpose a 4x4 matrix type.
{-@ transpose :: {v:[a] | len v == 16} -> [a] @-}
transpose :: [a] -> [a]
transpose [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] = 
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transpose _ = error "input to `transpose` must be a list of 16 objects"

-- |Convert a list of numbers to a list of strings. This is always possible. 
numberListToStringList :: [Word32] -> [String]
numberListToStringList = map (\x -> printf "%d" x)

-- |Convert a list of numbers to an list of `Either` type.
numberListToEitherList :: [Word32] -> [Either Word32 String]
numberListToEitherList = map Left

-- |Convert a list of strings to a list of `Either` type.
stringListToEitherList :: [String] -> [Either Word32 String]
stringListToEitherList = map Right

-- |Convert a list of `Either` type to an list of numbers.
eitherListToNumberList :: [Either Word32 String] -> [Word32]
eitherListToNumberList = map (fromLeft 0)

-- |Convert a list of `Either` type to a list of strings. 
eitherListToStringList :: [Either Word32 String] -> [String]
eitherListToStringList = map (fromRight "0")
