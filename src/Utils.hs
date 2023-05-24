module Utils
    (
        littleendian,
        littleendianInv,
        reduce,
        aument,
        modMatrix,
    ) where

import Data.Bits

import Data.Word
import Data.Tuple.Select

import Types (VectorType, MatrixType, Matrix64Type)

--
power :: Word32 -> Word32
power p = 2 ^ p 

--
littleendian :: VectorType -> Word32
littleendian (b0, b1, b2, b3) = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3

-- https://crypto.stackexchange.com/a/22314
littleendianInv ::  Word32 -> VectorType
littleendianInv w = (w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff)

--
reduce :: Matrix64Type -> MatrixType
reduce input = (
    (
        littleendian (sel1 (sel1 input)),
        littleendian (sel2 (sel1 input)),
        littleendian (sel3 (sel1 input)),
        littleendian (sel4 (sel1 input))
    ),
    (
        littleendian (sel1 (sel2 input)),
        littleendian (sel2 (sel2 input)),
        littleendian (sel3 (sel2 input)),
        littleendian (sel4 (sel2 input))
    ),
    (
        littleendian (sel1 (sel3 input)),
        littleendian (sel2 (sel3 input)),
        littleendian (sel3 (sel3 input)),
        littleendian (sel4 (sel3 input))
    ),
    (
        littleendian (sel1 (sel4 input)),
        littleendian (sel2 (sel4 input)),
        littleendian (sel3 (sel4 input)),
        littleendian (sel4 (sel4 input))
    ))

--
aument :: MatrixType -> Matrix64Type
aument input = (
    (
        littleendianInv (sel1 (sel1 input)),
        littleendianInv (sel2 (sel1 input)),
        littleendianInv (sel3 (sel1 input)),
        littleendianInv (sel4 (sel1 input))
    ),
    (
        littleendianInv (sel1 (sel2 input)),
        littleendianInv (sel2 (sel2 input)),
        littleendianInv (sel3 (sel2 input)),
        littleendianInv (sel4 (sel2 input))
    ),
    (
        littleendianInv (sel1 (sel3 input)),
        littleendianInv (sel2 (sel3 input)),
        littleendianInv (sel3 (sel3 input)),
        littleendianInv (sel4 (sel3 input))
    ),
    (
        littleendianInv (sel1 (sel4 input)),
        littleendianInv (sel2 (sel4 input)),
        littleendianInv (sel3 (sel4 input)),
        littleendianInv (sel4 (sel4 input))
    ))

--
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
