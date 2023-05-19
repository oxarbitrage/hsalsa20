module Doubleround
    (
    doubleround,
    doubleround10
    ) where

import Types (MatrixType)
import Rowround (rowround)
import Columnround (columnround)

-- The doubleround expression.
doubleround :: MatrixType -> MatrixType
doubleround = rowround . columnround

-- The doubleround10 expression.
doubleround10 :: MatrixType -> MatrixType
doubleround10 = doubleround . doubleround . doubleround . doubleround . doubleround
    . doubleround . doubleround . doubleround . doubleround . doubleround