module Utils
    (
        get0,
        get1,
        get2, 
        get3,
        getfirst,
        getsecond,
        getthird,
        getfourth
    ) where

import Data.Word

import Types (VectorType, MatrixType)

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

-- Utility function to get the first 4-Tuple of a 16-Tuple.
getfirst :: MatrixType -> VectorType
getfirst (a, _, _, _) = a

-- Utility function to get the second 4-Tuple of a 16-Tuple.
getsecond :: MatrixType -> VectorType
getsecond (_, a, _, _) = a

-- Utility function to get the third 4-Tuple of a 16-Tuple.
getthird :: MatrixType -> VectorType
getthird (_, _, a, _) = a

-- Utility function to get the third 4-Tuple of a 16-Tuple.
getfourth :: MatrixType -> VectorType
getfourth (_, _, _, a) = a
