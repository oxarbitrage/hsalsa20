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

import Data.Bits
import Data.Word

-- |Calculate an index over 64 as described in the spec.
iOver64 :: Double -> [Word32]
iOver64 i = littleendianInv4Crypt (floor (i / 64))

-- |Join the nonce and the calculated `iOver64`
nonceAndiOver64 :: Integral a => [Word32] -> a -> [Word32]
nonceAndiOver64 n i = n ++ iOver64 (fromIntegral i)

-- | Given a key, a nonce and an index get the salsa209 expanded matrix of it. 
crypt :: [Word32] -> [Word32] -> Int -> [Word32]
crypt k n i = expand16 k (nonceAndiOver64 n i)

-- | Given an aumented key and an index of it, returns the number corresponding to that index.
keybyte :: [Word32] -> Int -> Word32
keybyte aumented_key n = aumented_key!!n

-- | Encrypt or decrypt a message resulting in a list of the same length.
cryptBlock :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlock (x:xs) k n index = xor x (keybyte (crypt k n index) (index `mod` 64)) : 
    cryptBlock xs k n (index+1)
cryptBlock [] _ _ _ = []
