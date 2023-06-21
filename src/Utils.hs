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
        aument,
        modMatrix,
        littleendianInv4Crypt,
        stringListToNumberList,
        numberListToStringList,
        transpose,
    ) where

import Data.Bits
import Data.Word
import Data.List.Split
import Text.Printf

-- |Raise 2 to the power of `p`.
power :: Word32 -> Word32
power p = 2 ^ p 

-- |Encode a vector as a word using protocol specified littleendian. 
littleendian :: [Word32] -> Word32
littleendian [b0, b1, b2, b3] = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3
littleendian _ = 0

-- |The inverse of `littleendian`. Implemented as specified in https://crypto.stackexchange.com/a/22314.
littleendianInv :: Word32 -> [Word32]
littleendianInv w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff]

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
reduce :: [Word32] -> [Word32]
reduce input = map littleendian $ chunksOf 4 input

-- |Aument a matrix of 16 elements to one of 64 elements by using `littleendianInv`.
aument :: [Word32] -> [Word32]
aument = concatMap littleendianInv

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWith (+)

-- |The littleendian function used in encryption/decryption where a number is obtained given a list of 8 bytes.
littleendianInv4Crypt ::  Word32 -> [Word32]
littleendianInv4Crypt w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff,
    shiftR w 32 .&. 0xff, shiftR 40 8 .&. 0xff, shiftR w 48 .&. 0xff, shiftR w 56 .&. 0xff]

-- |Transpose a 4x4 matrix type.
transpose :: [a] -> [a]
transpose [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] = 
    [y0, y4, y8, y12, y1, y5, y9, y13, y2, y6, y10, y14, y3, y7, y11, y15]
transpose n = n

stringListToNumberList :: [String] -> [Word32]
stringListToNumberList = map (\x -> read x :: Word32)

numberListToStringList :: [Word32] -> [String]
numberListToStringList = map (\x -> printf "%d" x)
