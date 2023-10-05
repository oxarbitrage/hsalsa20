{-|
Module      : Keelang Utils
Description : Utilities
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

General utility functions to work with Keelung.
-}
module KeelungUtils
    (
        W32M, fromWord32List
    )
where

import Keelung
import Data.Word

-- Code borrowed from keelung-stdlib
-- https://github.com/btq-ag/keelung-stdlib/blob/main/src/Lib/W32M.hs

type W32M = ArrM Boolean

fromWord32 :: Word32 -> Comp W32M
fromWord32 word = toArrayM $ map (word !!!) [0 .. 31]

fromWord32List :: [Word32] -> Comp (ArrM W32M)
fromWord32List ws = mapM fromWord32 ws >>= toArrayM
