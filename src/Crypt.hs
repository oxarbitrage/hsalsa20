{-|
Module      : Crypt
Description : Salsa20 encryption and decryption
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

The salsa20 encryption and decryption.
-}
module Crypt
    (
    cryptBlock,
    ) where

import Expansion
import Utils
import Types (Matrix64Type, Vector8Type)

import Data.Bits
import Data.Word

calculateI :: RealFrac a => a -> Vector8Type
calculateI i = littleendianInv2 (floor (i / 64))

nonce_and_iover64 :: Integral a => [Word32] -> a -> [Word32]
nonce_and_iover64 n i = n ++ halfmatrix2list (calculateI (fromIntegral i))

crypt :: [Word32] -> [Word32] -> Int -> Matrix64Type
crypt k n i = expand16 (list2matrix k) (list2matrix (nonce_and_iover64 n i))

keybyte :: [Word32] -> Int -> Word32
keybyte aumented_key n = aumented_key!!n

cryptBlock :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlock (x:xs) k n index = xor x (keybyte (matrix642list (crypt k n index)) (index `mod` 64)) : 
    cryptBlock xs k n (index+1)
cryptBlock [] _ _ _ = []
