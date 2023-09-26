{-|
Module      : Utils
Description : Utilities
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

General utility functions used to create the salsa20 cipher.
-}
module Utils
    (
        littleendian,
        extractBytes,
        displayBytes,
        reduce,
        reduceDisplay,
        aument,
        aumentDisplay,
        modMatrix,
        numberListToStringList,
        transpose,
        modMatrixDisplay,
        numberListToEitherList,
        stringListToEitherList,
        eitherListToNumberList,
        eitherListToStringList,
    ) where

import Data.Bits
import Data.Word
import Data.List.Split
import Text.Printf
import Data.Either (fromLeft, fromRight)

import Operators()

-- | Encode a vector as a word using little-endian byte order.
littleendian :: [Word32] -> Word32
littleendian bytes = sum [byte `shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

-- |The littleendian expression as a string.
littleendianDisplay :: [String] -> String
littleendianDisplay [b0, b1, b2, b3] = printf "%s + 2^8 * %s + 2^16 * %s + 2^24 * %s" b0 b1 b2 b3
littleendianDisplay _ = ""

-- | Extract a specified number of bytes from a Word32.
extractBytes :: Int -> Word32 -> [Word32]
extractBytes numBytes w =
    [ shiftR w (8 * i) .&. 0xff | i <- [0 .. numBytes - 1] ]

-- | Display a specified number of bytes from a string as a list of strings.
displayBytes :: Int -> String -> [String]
displayBytes numBytes w =
    [ printf "%s & 255" w | _ <- [1 .. numBytes] ] ++
    [ printf "%d >>> %s & 255" (8 * i) w | i <- [numBytes .. 7] ]

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
reduce :: [Word32] -> [Word32]
reduce input = map littleendian $ chunksOf 4 input

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianDisplay` encoding.
reduceDisplay :: [String] -> [String]
reduceDisplay input = map littleendianDisplay $ chunksOf 4 input

-- |Aument a matrix of 16 elements to one of 64 elements by using `littleendianInv`.
aument :: [Word32] -> [Word32]
aument = concatMap $ extractBytes 4

-- |Aument a matrix of 16 elements to one of 64 elements by using `littleendianInvDisplay`.
aumentDisplay :: [String] -> [String]
aumentDisplay = concatMap $ displayBytes 4

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWith (+)

-- |Given two matrices, do modulo addition on each of the elements.
modMatrixDisplay :: [String] -> [String] -> [String]
modMatrixDisplay = zipWith displayMod

-- Append modulo addition symbol and a string to a given string.
displayMod :: String -> String -> String
displayMod a b = a ++ " + " ++ b

-- |Transpose a 4x4 matrix type.
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
