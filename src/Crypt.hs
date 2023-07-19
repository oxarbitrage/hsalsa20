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
    cryptBlockV1,
    cryptBlockV2,
    ) where

import Expansion
import Utils

import Data.Bits
import Data.Word

-- |Calculate an index over 64 as described in the spec.
iOver64 :: Integral a => a -> [Word32]
iOver64 i = littleendianInv4Crypt $ floor ((fromIntegral i / 64) :: Double)

-- |Join the nonce and the calculated `iOver64`
nonceAndiOver64 :: [Word32] -> Word8 -> [Word32]
nonceAndiOver64 n i = n ++ iOver64 i

-- |Given a single 16 bytes key, a nonce and an index get the salsa20 expanded matrix of it. 
cryptV1 :: [Word32] -> [Word32] -> Word8 -> [Word32]
cryptV1 k n i = expand16Compute k $ nonceAndiOver64 n i

-- |Given two 16 bytes keys, a nonce and an index get the salsa20 expanded matrix of it. 
cryptV2 :: [Word32] -> [Word32] -> [Word32] -> Word8 -> [Word32]
cryptV2 k0 k1 n i = expand32Compute k0 k1 $ nonceAndiOver64 n i

-- |Given an aumented key and an index of it, returns the number corresponding to that index.
keybyte :: [Word32] -> Int -> Word32
keybyte aumented_key n = aumented_key!!n

-- |Encrypt or decrypt a message with a single 16 bytes key resulting in a list of the same length.
cryptBlockV1 :: [Word32] -> [Word32] -> [Word32] -> Word8 -> [Word32]
cryptBlockV1 (x:xs) k n index = xor x (keybyte (cryptV1 k n index) (fromIntegral index `mod` 64)) :
    cryptBlockV1 xs k n (index+1)
cryptBlockV1 _ _ _ _ = []

-- |Encrypt or decrypt a message with two 16 bytes key resulting in a list of the same length.
cryptBlockV2 :: [Word32] -> [Word32] -> [Word32] -> [Word32] -> Word8 -> [Word32]
cryptBlockV2 (x:xs) k0 k1 n index = xor x (keybyte (cryptV2 k0 k1 n index) (fromIntegral index `mod` 64)) : 
    cryptBlockV2 xs k0 k1 n (index+1)
cryptBlockV2 _ _ _ _ _ = []
