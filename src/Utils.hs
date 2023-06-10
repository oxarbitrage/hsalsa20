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
        littleendianInv2,
    ) where

import Data.Bits
import Data.Word

-- |Raise 2 to the power of `p`.
power :: Word32 -> Word32
power p = 2 ^ p 

-- |Encode a vector as a word using protocol specified littleendian. 
littleendian :: [Word32] -> Word32
littleendian [b0, b1, b2, b3] = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3
littleendian _ = 0

-- |The inverse of `littleendian`. Implemented as specified in https://crypto.stackexchange.com/a/22314.
littleendianInv ::  Word32 -> [Word32]
littleendianInv w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff]

-- Reduce a matrix of 64 elements to a matrix of 32 elements by using `littleendian` encoding.
reduce :: [Word32] -> [Word32]
reduce input = [
    littleendian [input!!0, input!!1, input!!2, input!!3],
    littleendian [input!!4, input!!5, input!!6, input!!7],
    littleendian [input!!8, input!!9, input!!10, input!!11],
    littleendian [input!!12, input!!13, input!!14, input!!15],
    littleendian [input!!16, input!!17, input!!18, input!!19],
    littleendian [input!!20, input!!21, input!!22, input!!23],
    littleendian [input!!24, input!!25, input!!26, input!!27],
    littleendian [input!!28, input!!29, input!!30, input!!31],
    littleendian [input!!32, input!!33, input!!34, input!!35],
    littleendian [input!!36, input!!37, input!!38, input!!39],
    littleendian [input!!40, input!!41, input!!42, input!!43],
    littleendian [input!!44, input!!45, input!!46, input!!47],
    littleendian [input!!48, input!!49, input!!50, input!!51],
    littleendian [input!!52, input!!53, input!!54, input!!55],
    littleendian [input!!56, input!!57, input!!58, input!!59],
    littleendian [input!!60, input!!61, input!!62, input!!63]
    ]

-- Aument a matrix of 32 elements to one of 64 elements by using `littleendianInv`.
aument :: [Word32] -> [Word32]
aument input = concat [
        littleendianInv $ input!!0,
        littleendianInv $ input!!1,
        littleendianInv $ input!!2,
        littleendianInv $ input!!3,
        littleendianInv $ input!!4,
        littleendianInv $ input!!5,
        littleendianInv $ input!!6,
        littleendianInv $ input!!7,
        littleendianInv $ input!!8,
        littleendianInv $ input!!9,
        littleendianInv $ input!!10,
        littleendianInv $ input!!11,
        littleendianInv $ input!!12,
        littleendianInv $ input!!13,
        littleendianInv $ input!!14,
        littleendianInv $ input!!15]

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWith (+)

--
littleendianInv2 ::  Word32 -> [Word32]
littleendianInv2 w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff,
    shiftR w 32 .&. 0xff, shiftR 40 8 .&. 0xff, shiftR w 48 .&. 0xff, shiftR w 56 .&. 0xff]
