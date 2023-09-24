{-|
Module      : Operators
Description : Custom operators
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

Custom operators.

-}
module Operators
    (
    (>>>),
    (&),
    (⊕),
    (<<<),
    ) where

import Data.Bits
import Data.Word

-- |The shiftR operator.
(>>>) :: Word32 -> Int -> Word32
(>>>) = shiftR

-- |The bitwise and operator.
(&) :: Word32 -> Word32 -> Word32
(&) a b = a .&. b

-- |The xor operator.
(⊕) :: Word32 -> Word32 -> Word32
(⊕) = xor

-- |The shift operator.
(<<<) :: Word32 -> Int -> Word32
(<<<) = rotate
