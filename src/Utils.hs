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
        list2matrix,
        halfmatrix2list,
        matrix642list,
    ) where

import Data.Bits

import Data.Word
import Data.Tuple.Select

import Types (VectorType, MatrixType, Matrix64Type, Vector8Type)

-- |Raise 2 to the power of `p`.
power :: Word32 -> Word32
power p = 2 ^ p 

-- |Encode a vector as a word using protocol specified littleendian. 
littleendian :: VectorType -> Word32
littleendian (b0, b1, b2, b3) = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3

-- |The inverse of `littleendian`. Implemented as specified in https://crypto.stackexchange.com/a/22314.
littleendianInv ::  Word32 -> VectorType
littleendianInv w = (w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff)

-- Reduce a matrix of 64 elements to a matrix of 32 elements by using `littleendian` encoding.
reduce :: Matrix64Type -> MatrixType
reduce input = (
    (
        littleendian $ sel1 $ sel1 input,
        littleendian $ sel2 $ sel1 input,
        littleendian $ sel3 $ sel1 input,
        littleendian $ sel4 $ sel1 input
    ),
    (
        littleendian $ sel1 $ sel2 input,
        littleendian $ sel2 $ sel2 input,
        littleendian $ sel3 $ sel2 input,
        littleendian $ sel4 $ sel2 input
    ),
    (
        littleendian $ sel1 $ sel3 input,
        littleendian $ sel2 $ sel3 input,
        littleendian $ sel3 $ sel3 input,
        littleendian $ sel4 $ sel3 input
    ),
    (
        littleendian $ sel1 $ sel4 input,
        littleendian $ sel2 $ sel4 input,
        littleendian $ sel3 $ sel4 input,
        littleendian $ sel4 $ sel4 input
    ))

-- Aument a matrix of 32 elements to one of 64 elements by using `littleendianInv`.
aument :: MatrixType -> Matrix64Type
aument input = (
    (
        littleendianInv $ sel1 $ sel1 input,
        littleendianInv $ sel2 $ sel1 input,
        littleendianInv $ sel3 $ sel1 input,
        littleendianInv $ sel4 $ sel1 input
    ),
    (
        littleendianInv $ sel1 $ sel2 input,
        littleendianInv $ sel2 $ sel2 input,
        littleendianInv $ sel3 $ sel2 input,
        littleendianInv $ sel4 $ sel2 input
    ),
    (
        littleendianInv $ sel1 $ sel3 input,
        littleendianInv $ sel2 $ sel3 input,
        littleendianInv $ sel3 $ sel3 input,
        littleendianInv $ sel4 $ sel3 input
    ),
    (
        littleendianInv $ sel1 $ sel4 input,
        littleendianInv $ sel2 $ sel4 input,
        littleendianInv $ sel3 $ sel4 input,
        littleendianInv $ sel4 $ sel4 input
    ))

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: MatrixType -> MatrixType -> MatrixType
modMatrix inputL inputR = (
    (
        sel1 (sel1 inputL) + sel1 (sel1 inputR),
        sel2 (sel1 inputL) + sel2 (sel1 inputR),
        sel3 (sel1 inputL) + sel3 (sel1 inputR),
        sel4 (sel1 inputL) + sel4 (sel1 inputR)
    ),
    (
        sel1 (sel2 inputL) + sel1 (sel2 inputR),
        sel2 (sel2 inputL) + sel2 (sel2 inputR),
        sel3 (sel2 inputL) + sel3 (sel2 inputR),
        sel4 (sel2 inputL) + sel4 (sel2 inputR)
    ),
    (
        sel1 (sel3 inputL) + sel1 (sel3 inputR),
        sel2 (sel3 inputL) + sel2 (sel3 inputR),
        sel3 (sel3 inputL) + sel3 (sel3 inputR),
        sel4 (sel3 inputL) + sel4 (sel3 inputR)
    ),
    (
        sel1 (sel4 inputL) + sel1 (sel4 inputR),
        sel2 (sel4 inputL) + sel2 (sel4 inputR),
        sel3 (sel4 inputL) + sel3 (sel4 inputR),
        sel4 (sel4 inputL) + sel4 (sel4 inputR)
    ))

--
littleendianInv2 ::  Word32 -> Vector8Type
littleendianInv2 w = ((w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff),
    (shiftR w 32 .&. 0xff, shiftR 40 8 .&. 0xff, shiftR w 48 .&. 0xff, shiftR w 56 .&. 0xff))

-- conversion functions from my types to list, this is a pain, i will probaby move everythign to list.

list2matrix :: [Word32] -> MatrixType 
list2matrix l = (
    (head l, l!!1, l!!2, l!!3),
    (l!!4, l!!5, l!!6, l!!7),
    (l!!8, l!!9, l!!10, l!!11),
    (l!!12, l!!13, l!!14, l!!15)
    ) 

halfmatrix2list :: Vector8Type -> [Word32] 
halfmatrix2list v = [sel1 (sel1 v), sel2 (sel1 v), sel3 (sel1 v), sel4 (sel1 v),
    sel1 (sel2 v), sel2 (sel2 v), sel3 (sel2 v), sel4 (sel2 v)]

matrix642list :: Matrix64Type -> [Word32]
matrix642list m = [
    sel1 (sel1 (sel1 m)),
    sel1 (sel1 (sel2 m)),
    sel1 (sel1 (sel3 m)),
    sel1 (sel1 (sel4 m)),

    sel1 (sel2 (sel1 m)),
    sel1 (sel2 (sel2 m)),
    sel1 (sel2 (sel3 m)),
    sel1 (sel2 (sel4 m)),

    sel1 (sel3 (sel1 m)),
    sel1 (sel3 (sel2 m)),
    sel1 (sel3 (sel3 m)),
    sel1 (sel3 (sel4 m)),

    sel1 (sel4 (sel1 m)),
    sel1 (sel4 (sel2 m)),
    sel1 (sel4 (sel3 m)),
    sel1 (sel4 (sel4 m)),

    --
    sel2 (sel1 (sel1 m)),
    sel2 (sel1 (sel2 m)),
    sel2 (sel1 (sel3 m)),
    sel2 (sel1 (sel4 m)),

    sel2 (sel2 (sel1 m)),
    sel2 (sel2 (sel2 m)),
    sel2 (sel2 (sel3 m)),
    sel2 (sel2 (sel4 m)),

    sel2 (sel3 (sel1 m)),
    sel2 (sel3 (sel2 m)),
    sel2 (sel3 (sel3 m)),
    sel2 (sel3 (sel4 m)),

    sel2 (sel4 (sel1 m)),
    sel2 (sel4 (sel2 m)),
    sel2 (sel4 (sel3 m)),
    sel2 (sel4 (sel4 m)),

    --
    sel3 (sel1 (sel1 m)),
    sel3 (sel1 (sel2 m)),
    sel3 (sel1 (sel3 m)),
    sel3 (sel1 (sel4 m)),

    sel3 (sel2 (sel1 m)),
    sel3 (sel2 (sel2 m)),
    sel3 (sel2 (sel3 m)),
    sel3 (sel2 (sel4 m)),

    sel3 (sel3 (sel1 m)),
    sel3 (sel3 (sel2 m)),
    sel3 (sel3 (sel3 m)),
    sel3 (sel3 (sel4 m)),

    sel3 (sel4 (sel1 m)),
    sel3 (sel4 (sel2 m)),
    sel3 (sel4 (sel3 m)),
    sel3 (sel4 (sel4 m)),

    --
    sel4 (sel1 (sel1 m)),
    sel4 (sel1 (sel2 m)),
    sel4 (sel1 (sel3 m)),
    sel4 (sel1 (sel4 m)),

    sel4 (sel2 (sel1 m)),
    sel4 (sel2 (sel2 m)),
    sel4 (sel2 (sel3 m)),
    sel4 (sel2 (sel4 m)),

    sel4 (sel3 (sel1 m)),
    sel4 (sel3 (sel2 m)),
    sel4 (sel3 (sel3 m)),
    sel4 (sel3 (sel4 m)),

    sel4 (sel4 (sel1 m)),
    sel4 (sel4 (sel2 m)),
    sel4 (sel4 (sel3 m)),
    sel4 (sel4 (sel4 m))]
