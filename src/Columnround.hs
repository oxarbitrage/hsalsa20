module Columnround
    (
    columnround
    ) where

import Types (MatrixType)
import Rowround (rowround)

transpose :: MatrixType -> MatrixType
transpose ((y0, y1, y2, y3), (y4, y5, y6, y7), (y8, y9, y10, y11), (y12, y13, y14, y15)) = 
    ((y0, y4, y8, y12), (y1, y5, y9, y13), (y2, y6, y10, y14), (y3, y7, y11, y15)) 

transpose_inv :: MatrixType -> MatrixType
transpose_inv ((x0, x4, x8, x12), (x1, x5, x9, x13), (x2, x6, x10, x14), (x3, x7, x11, x15)) =
    ((x0, x1, x2, x3), (x4, x5, x6, x7), (x8, x9, x10, x11), (x12, x13, x14, x15))

-- The rowround expression.
columnround :: MatrixType -> MatrixType
columnround input = transpose_inv (rowround (transpose input))
