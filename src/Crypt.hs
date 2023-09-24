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
    iOver64Display,
    nonceAndiOver64Display,
    cryptV2,
    cryptV1Display,
    cryptV2Display,
    cryptBlockV1Display,
    cryptBlockV1Equations,
    iOver64,
    nonceAndiOver64,
    ) where

import Expansion
import Utils

import Data.Bits
import Data.Word
import Text.Printf

{- |Calculate an index over 64 as described in the spec.
`i` is the index of a provided l-length message that we want to encrypt. This index is then divided by 64 and the
floor of it is taken.
The resulting  number is returned expressed as a unique 8 bytes sequence.
-}
iOver64 :: Integral a => a -> [Word32]
iOver64 i = littleendianInv4Crypt $ floor ((fromIntegral i / 64) :: Double)

-- |Display the calculation of an index over 64. See `iOver64`.
iOver64Display :: String -> [String]
iOver64Display i = littleendianInv4CryptDisplay $ printf "_(%s/64)" i

{- |Join the nonce and the calculated `iOver64`.
Nonce is 8 bytes and index 8 bytes more for a total of 16 as the result.
-}
nonceAndiOver64 :: [Word32] -> Int -> [Word32]
nonceAndiOver64 n i = n ++ iOver64 i

-- |Join the nonce and the calculated `iOver64Display`.
nonceAndiOver64Display :: [String] -> String -> [String]
nonceAndiOver64Display n i = n ++ iOver64Display i

-- |Given a single 16 bytes key, a nonce and an index of a message byte, get the salsa20 expanded matrix of it. 
cryptV1 :: [Word32] -> [Word32] -> Int -> [Word32]
cryptV1 k n i = expand16Compute k $ nonceAndiOver64 n i

-- |Given a single 16 bytes key, a nonce and an index get the salsa20 expanded matrix of it.
cryptV1Display :: [String] -> [String] -> String -> [String]
cryptV1Display k n i = expand16Display k $ nonceAndiOver64Display n i

-- |Given two 16 bytes keys, a nonce and an index get the salsa20 expanded matrix of it.
cryptV2 :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptV2 k0 k1 n i = expand32Compute k0 k1 $ nonceAndiOver64 n i

-- |Given two 16 bytes keys, a nonce and an index get the salsa20 expanded matrix of it.
cryptV2Display :: [String] -> [String] -> [String] -> String -> [String]
cryptV2Display k0 k1 n i = expand32Display k0 k1 $ nonceAndiOver64Display n i

-- |Given an aumented key and an index of it, returns the number corresponding to that index.
keybyte :: [Word32] -> Int -> Word32
keybyte aumented_key n = aumented_key!!n

-- |Given an aumented key as a list of strings and an index of it, returns the string corresponding to that index.
keybyteDisplay :: [String] -> Int -> String
keybyteDisplay aumented_key n = aumented_key!!n

-- |Encrypt or decrypt a message with a single 16 bytes key resulting in a list of the same length.
cryptBlockV1 :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlockV1 (x:xs) k n index = xor x (keybyte (cryptV1 k n index) (index `mod` 64)) :
    cryptBlockV1 xs k n (index+1)
cryptBlockV1 _ _ _ _ = []

-- |Encrypt or decrypt a message with a single 16 bytes key resulting in a list of the same length.
cryptBlockV1Display :: [String] -> [String] -> [String] -> Int -> [String]
cryptBlockV1Display (x:xs) k n index = printf "%s ⊕ %s" x (keybyteDisplay (cryptV1Display k n (show index)) (index `mod` 64)) :
    cryptBlockV1Display xs k n (index+1)
cryptBlockV1Display _ _ _ _ = []

-- |Display the output of `cryptBlockV1` as string equations.
cryptBlockV1Equations :: [String] -> [String] -> [String] -> Int -> [String]
cryptBlockV1Equations m k n index = do
    let display = cryptBlockV1Display m k n index
    let displayIndex = zip [0 :: Int ..] display
    let equation = map (uncurry (printf "c%d = %s")) displayIndex
    equation

-- |Encrypt or decrypt a message with two 16 bytes key resulting in a list of the same length.
cryptBlockV2 :: [Word32] -> [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlockV2 (x:xs) k0 k1 n index = xor x (keybyte (cryptV2 k0 k1 n index) (index `mod` 64)) :
    cryptBlockV2 xs k0 k1 n (index+1)
cryptBlockV2 _ _ _ _ _ = []
