module Main (main) where

import Quarterround
import Data.Word

import Text.Printf

input :: (Word32, Word32, Word32, Word32)
input = (0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137)

z0 :: Word32
z0 = get0 (quarterround input)
z1 :: Word32
z1 = get1 (quarterround input)
z2 :: Word32
z2 = get2 (quarterround input)
z3 :: Word32
z3 = get3 (quarterround input)

main :: IO ()
main = printf "0x%08x, 0x%08x, 0x%08x, 0x%08x" z0 z1 z2 z3
