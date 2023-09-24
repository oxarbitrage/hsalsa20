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
        littleendianInv,
        reduce,
        reduceDisplay,
        aument,
        aumentDisplay,
        modMatrix,
        littleendianInv4Crypt,
        littleendianInv4CryptDisplay,
        stringListToNumberList,
        numberListToStringList,
        transpose,
        index0,
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

-- |Raise 2 to the power of `p`.
power :: Word32 -> Word32
power p = 2 ^ p

-- |Encode a vector as a word using protocol specified littleendian. 
littleendian :: [Word32] -> Word32
littleendian [b0, b1, b2, b3] = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3
littleendian _ = 0

-- |The littleendian expression as a string.
littleendianDisplay :: [String] -> String
littleendianDisplay [b0, b1, b2, b3] = printf "%s + 2^8 * %s + 2^16 * %s + 2^24 * %s" b0 b1 b2 b3
littleendianDisplay _ = ""

-- |The inverse of `littleendian`. Implemented as specified in https://crypto.stackexchange.com/a/22314.
littleendianInv :: Word32 -> [Word32]
littleendianInv w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff]

-- |The inverse of `littleendian` as a string.
littleendianInvDisplay :: String -> [String]
littleendianInvDisplay w = [
    printf "%s & 255" w,
    printf "8 >>> %s & 255" w,
    printf "16 >>> %s & 255" w,
    printf "24 >>> %s & 255" w
    ]

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
reduce :: [Word32] -> [Word32]
reduce input = map littleendian $ chunksOf 4 input

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianDisplay` encoding.
reduceDisplay :: [String] -> [String]
reduceDisplay input = map littleendianDisplay $ chunksOf 4 input

-- |Aument a matrix of 16 elements to one of 64 elements by using `littleendianInv`.
aument :: [Word32] -> [Word32]
aument = concatMap littleendianInv

-- |Aument a matrix of 16 elements to one of 64 elements by using `littleendianInvDisplay`.
aumentDisplay :: [String] -> [String]
aumentDisplay = concatMap littleendianInvDisplay

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWith (+)

-- |Given two matrices, do modulo addition on each of the elements.
modMatrixDisplay :: [String] -> [String] -> [String]
modMatrixDisplay = zipWith displayMod

-- Append modulo addition symbol and a string to a given string.
displayMod :: String -> String -> String
displayMod a b = a ++ " + " ++ b

-- |The littleendian function used in encryption/decryption where a list of 8 bytes is obtained from a number.
littleendianInv4Crypt ::  Word32 -> [Word32]
littleendianInv4Crypt w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff,
    shiftR w 32 .&. 0xff, shiftR 40 8 .&. 0xff, shiftR w 48 .&. 0xff, shiftR w 56 .&. 0xff]

-- |The `littleendianInv4Crypt` as a string.
littleendianInv4CryptDisplay :: String -> [String]
littleendianInv4CryptDisplay w = [
    printf "%s & 255" w,
    printf "8 >>> %s & 255" w,
    printf "16 >>> %s & 255" w,
    printf "24 >>> %s & 255" w,
    printf "32 >>> %s & 255" w,
    printf "40 >>> %s & 255" w,
    printf "48 >>> %s & 255" w,
    printf "56 >>> %s & 255" w
    ]

-- |Transpose a 4x4 matrix type.
transpose :: [a] -> [a]
transpose [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] = 
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transpose _ = error "input to `transpose` must be a list of 16 objects"

-- |Convert a list of strings to a list of numbers if possible.
stringListToNumberList :: [String] -> [Word32]
stringListToNumberList = map (\x -> read x :: Word32)

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

-- |The number 0 as an integer. Used as list index in certain cases.
index0 :: Int
index0 = 0
