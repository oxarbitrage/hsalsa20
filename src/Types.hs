module Types
    (
        VectorType,
        MatrixType,
        Matrix64Type,
    ) where

import Data.Word

-- We define an alias for a 4-Tuple of Word32 
type VectorType = (Word32, Word32, Word32, Word32)

-- We define an alias for a 16-Tuple of Word32 objects.
type MatrixType = (VectorType, VectorType, VectorType, VectorType)

-- We define an alias for a 64-Tuple of Word32 objects.
type Matrix64Type = (MatrixType, MatrixType, MatrixType, MatrixType)
