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
iOver64 index = extractBytes 8 $ floor (fromIntegral index / 64 :: Double)

-- |Display the calculation of an index over 64. See `iOver64`.
iOver64Display :: String -> [String]
-- TODO: is this the same?
--iOver64Display index = [printf "_(%s/64)" index]
iOver64Display index = displayBytes 8 $ printf "_(%s/64)" index

{- |Join the nonce and the calculated `iOver64`.
Nonce is 8 bytes and index 8 bytes more for a total of 16 as the result.
-}
nonceAndiOver64 :: [Word32] -> Int -> [Word32]
nonceAndiOver64 nonce index
    | length nonce == 8 = nonce ++ iOver64 index
    | otherwise = error "First input to `nonceAndiOver64` must be a list of 8 `Word32` numbers"

-- |Join the nonce and the calculated `iOver64Display`.
nonceAndiOver64Display :: [String] -> String -> [String]
nonceAndiOver64Display nonce index
    | length nonce == 8 = nonce ++ iOver64Display index
    | otherwise = error "first input to `nonceAndiOver64Display` must be a list of 8 `String` strings"

-- |Given a single 16 bytes key, a nonce and an index of a message byte, get the salsa20 expanded matrix of it. 
cryptV1 :: [Word32] -> [Word32] -> Int -> [Word32]
cryptV1 key nonce index
    | length key == 16 && length nonce == 8 = expand16Compute key $ nonceAndiOver64 nonce index
    | otherwise = error "first input to `cryptV1` must be a list of 16 `Word32` numbers and the second a list of 8 `Word32` numbers"

-- |Given a single 16 bytes key, a nonce and an index get the salsa20 expanded matrix of it.
cryptV1Display :: [String] -> [String] -> String -> [String]
cryptV1Display key nonce index
    | length key == 16 && length nonce == 8 = expand16Display key $ nonceAndiOver64Display nonce index
    | otherwise = error "first input to `cryptV1Display` must be a list of 16 `String` strings and the second a list of 8 `String` strings"

-- |Given two 16 bytes keys, a nonce and an index get the salsa20 expanded matrix of it.
cryptV2 :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptV2 key0 key1 nonce index 
    | length key0 == 16 && length key1 == 16 && length nonce == 8 = expand32Compute key0 key1 $ nonceAndiOver64 nonce index
    | otherwise = error "first input to `cryptV2` must be a list of 16 `Word32` numbers, the second a list of 16 `Word32` numbers and the third a list of 8 `Word32` numbers"

-- |Given two 16 bytes keys, a nonce and an index get the salsa20 expanded matrix of it.
cryptV2Display :: [String] -> [String] -> [String] -> String -> [String]
cryptV2Display key0 key1 nonce index
    | length key0 == 16 && length key1 == 16 && length nonce == 8 = expand32Display key0 key1 $ nonceAndiOver64Display nonce index
    | otherwise = error "first input to `cryptV2Display` must be a list of 16 `String` strings, the second a list of 16 `String` strings and the third a list of 8 `String` strings"

-- |Given an aumented key and an index of it, returns the number corresponding to that index.
keybyte :: [Word32] -> Int -> Word32
keybyte aumented_key index
    | length aumented_key == 64 = aumented_key!!index
    | otherwise = error "first input to `keybyte` must be a list of 64 `Word32` numbers"

-- |Given an aumented key as a list of strings and an index of it, returns the string corresponding to that index.
keybyteDisplay :: [String] -> Int -> String
keybyteDisplay aumented_key index
    | length aumented_key == 64 = aumented_key!!index
    | otherwise = error "first input to `keybyteDisplay` must be a list of 64 `String` strings"

-- |Encrypt or decrypt a message with a single 16 bytes key resulting in a list of the same length.
cryptBlockV1 :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlockV1 (x:xs) key nonce index
    | length key == 16 && length nonce == 8 = xor x (keybyte (cryptV1 key nonce index) (index `mod` 64)) :
        cryptBlockV1 xs key nonce (index+1)
    | otherwise = error "first input to `cryptBlockV1` must be a list of 16 `Word32` numbers and the second a list of 8 `Word32` numbers"
cryptBlockV1 _ _ _ _ = []

-- |Encrypt or decrypt a message with a single 16 bytes key resulting in a list of the same length.
cryptBlockV1Display :: [String] -> [String] -> [String] -> Int -> [String]
cryptBlockV1Display (x:xs) key nonce index
    | length key == 16 && length nonce == 8 = printf "%s ⊕ %s" x (keybyteDisplay (cryptV1Display key nonce (show index)) (index `mod` 64)) :
        cryptBlockV1Display xs key nonce (index+1)
    | otherwise = error "first input to `cryptBlockV1Display` must be a list of 16 `String` strings and the second a list of 8 `String` strings"
cryptBlockV1Display _ _ _ _ = []

-- |Display the output of `cryptBlockV1` as string equations.
cryptBlockV1Equations :: [String] -> [String] -> [String] -> Int -> [String]
cryptBlockV1Equations message key nonce index
    | length key == 16 && length nonce == 8 =
        [printf "z%d = %s" (idx :: Int) eq | (idx, eq) <- zip [0..] (cryptBlockV1Display message key nonce index)]
    | otherwise = error "first input to `cryptBlockV1Equations` must be a list of 16 `String` strings and the second a list of 8 `String` strings"

-- |Encrypt or decrypt a message with two 16 bytes key resulting in a list of the same length.
cryptBlockV2 :: [Word32] -> [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlockV2 (x:xs) key0 key1 nonce index
    | length key0 == 16 && length key1 == 16 && length nonce == 8 =
        xor x (keybyte (cryptV2 key0 key1 nonce index) (index `mod` 64)) :
            cryptBlockV2 xs key0 key1 nonce (index+1)
    | otherwise = error "first input to `cryptBlockV2` must be a list of 16 `Word32` numbers, the second a list of 16 `Word32` numbers and the third a list of 8 `Word32` numbers"
cryptBlockV2 _ _ _ _ _ = []
