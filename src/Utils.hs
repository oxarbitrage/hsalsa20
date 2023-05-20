module Utils
    (
        get0,
        get1,
        get2, 
        get3,
        getfirst,
        getsecond,
        getthird,
        getfourth,
        getfirst64,
        getsecond64,
        getthird64,
        getfourth64,
        littleendian,
        littleendianInv,
        reduce,
        aument,
        modMatrix,
    ) where

import Data.Bits

import Data.Word

import Types (VectorType, MatrixType, Matrix64Type)

-- Get the object in the first position of a 4-Tuple.
get0 :: VectorType -> Word32
get0 (a, _, _, _) = a

-- Get the object in the second position of a 4-Tuple.
get1 :: VectorType -> Word32
get1 (_, a, _, _) = a

-- Get the object in the third position of a 4-Tuple.
get2 :: VectorType -> Word32
get2 (_, _, a, _) = a

-- Get the object in the fourth position of a 4-Tuple.
get3 :: VectorType -> Word32
get3 (_, _, _, a) = a

-- Get the first 4-Tuple of a 16-Tuple.
getfirst :: MatrixType -> VectorType
getfirst (a, _, _, _) = a

-- Get the second 4-Tuple of a 16-Tuple.
getsecond :: MatrixType -> VectorType
getsecond (_, a, _, _) = a

-- Get the third 4-Tuple of a 16-Tuple.
getthird :: MatrixType -> VectorType
getthird (_, _, a, _) = a

-- Get the third 4-Tuple of a 16-Tuple.
getfourth :: MatrixType -> VectorType
getfourth (_, _, _, a) = a

--
getfirst64 :: Matrix64Type -> MatrixType 
getfirst64 (a, _, _, _) = a

--
getsecond64 :: Matrix64Type -> MatrixType 
getsecond64 (_, a, _, _) = a

--
getthird64 :: Matrix64Type -> MatrixType 
getthird64 (_, _, a, _) = a

--
getfourth64 :: Matrix64Type -> MatrixType 
getfourth64 (_, _, _, a) = a

power :: Word32 -> Word32
power p = 2 ^ p 

littleendian :: VectorType -> Word32
littleendian (b0, b1, b2, b3) = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3

-- https://crypto.stackexchange.com/a/22314
littleendianInv ::  Word32 -> VectorType
littleendianInv w = (w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff)

--
reduce :: Matrix64Type -> MatrixType
reduce input = (
    (
        littleendian (getfirst (getfirst64 input)),
        littleendian (getsecond (getfirst64 input)),
        littleendian (getthird (getfirst64 input)),
        littleendian (getfourth (getfirst64 input))
    ),
    (
        littleendian (getfirst (getsecond64 input)),
        littleendian (getsecond (getsecond64 input)),
        littleendian (getthird (getsecond64 input)),
        littleendian (getfourth (getsecond64 input))
    ),
    (
        littleendian (getfirst (getthird64 input)),
        littleendian (getsecond (getthird64 input)),
        littleendian (getthird (getthird64 input)),
        littleendian (getfourth (getthird64 input))
    ),
    (
        littleendian (getfirst (getfourth64 input)),
        littleendian (getsecond (getfourth64 input)),
        littleendian (getthird (getfourth64 input)),
        littleendian (getfourth (getfourth64 input))
    ))

--
aument :: MatrixType -> Matrix64Type
aument input = (
    (
        littleendianInv (get0 (getfirst input)),
        littleendianInv (get1 (getfirst input)),
        littleendianInv (get2 (getfirst input)),
        littleendianInv (get3 (getfirst input))
    ),
    (
        littleendianInv (get0 (getsecond input)),
        littleendianInv (get1 (getsecond input)),
        littleendianInv (get2 (getsecond input)),
        littleendianInv (get3 (getsecond input))
    ),
    (
        littleendianInv (get0 (getthird input)),
        littleendianInv (get1 (getthird input)),
        littleendianInv (get2 (getthird input)),
        littleendianInv (get3 (getthird input))
    ),
    (
        littleendianInv (get0 (getfourth input)),
        littleendianInv (get1 (getfourth input)),
        littleendianInv (get2 (getfourth input)),
        littleendianInv (get3 (getfourth input))
    ))

--
modMatrix :: MatrixType -> MatrixType -> MatrixType
modMatrix inputL inputR = (
    (
        get0 (getfirst inputL) + get0 (getfirst inputR),
        get1 (getfirst inputL) + get1 (getfirst inputR),
        get2 (getfirst inputL) + get2 (getfirst inputR),
        get3 (getfirst inputL) + get3 (getfirst inputR)
    ),
    (
        get0 (getsecond inputL) + get0 (getsecond inputR),
        get1 (getsecond inputL) + get1 (getsecond inputR),
        get2 (getsecond inputL) + get2 (getsecond inputR),
        get3 (getsecond inputL) + get3 (getsecond inputR)
    ),
    (
        get0 (getthird inputL) + get0 (getthird inputR),
        get1 (getthird inputL) + get1 (getthird inputR),
        get2 (getthird inputL) + get2 (getthird inputR),
        get3 (getthird inputL) + get3 (getthird inputR)
    ),
    (
        get0 (getfourth inputL) + get0 (getfourth inputR),
        get1 (getfourth inputL) + get1 (getfourth inputR),
        get2 (getfourth inputL) + get2 (getfourth inputR),
        get3 (getfourth inputL) + get3 (getfourth inputR)
    ))
