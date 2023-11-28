{-|
Module      : Crypt
Description : Salsa20 encryption and decryption
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

The 'Crypt' module provides functions for Salsa20 encryption and decryption. Salsa20 is a symmetric key stream cipher designed for fast and secure encryption of data. 
This module includes functions to calculate an index over 64, generate Salsa20 expansion matrices, 
encrypt or decrypt messages using 16-byte and 32-byte keys, and display encryption details as strings or equations.
The module ensures the uniqueness and security of encryption keys by utilizing nonces and augmented keys.

The Salsa20 algorithm is widely used in various cryptographic applications due to its strong security and high performance. 
Users can employ the functions provided by this module to secure their data with Salsa20 encryption and, if needed,
perform decryption to retrieve the original content.

Please refer to the individual function documentation for specific details and usage guidelines.
-}
module Crypt
    (
        cryptBlock16Compute, cryptBlock16Display,
        cryptBlock32Compute, cryptBlock32Display,
    )
where

import Expansion
import Utils

import Data.Bits
import Data.Word
import Text.Printf

{-|
Calculate an index over the scalar 64 as described in the spec. 
Given an index `i` representing the position of a message in a sequence of length `l`,
this function computes the floor of `i / 64` and expresses the result as a unique sequence of 8 bytes.
The resulting sequence is used in the Salsa20 encryption process.
-}
{-@ iOver64Compute :: Nat -> { o:[_] | (len o) == 8 }  @-}
iOver64Compute :: Int -> [Word32]
iOver64Compute index = extractBytes8 $ floor (fromIntegral index / 64 :: Double)

-- |Display the calculation of an index over the scalar 64. See `iOver64Compute`.
{-@ iOver64Display :: _ -> { o:[_] | (len o) == 8 }  @-}
iOver64Display :: String -> [String]
-- TODO: is this the same?
--iOver64Display index = [printf "_(%s/64)" index]
iOver64Display index = displayBytes8 $ printf "_(%s/64)" index

{-|
Join the nonce with the calculated `iOver64Compute` to create an extended nonce.
The `nonce` is an 8-byte sequence, and the `iOver64Compute` function produces an additional 8-byte sequence.
When combined, they form a 16-byte extended nonce. This extended nonce is used in Salsa20 encryption to ensure the
uniqueness of the encryption keys.
-}
{-@ nonceAndiOver64Compute :: { i:[_] | (len i) == 8 } -> Nat -> { o:[_] | (len o) == 16 }  @-}
nonceAndiOver64Compute :: [Word32] -> Int -> [Word32]
nonceAndiOver64Compute nonce index
    | length nonce == 8 = nonce ++ iOver64Compute index
    | otherwise = error "First input to `nonceAndiOver64Compute` must be a list of 8 `Word32` numbers"

-- |Join the nonce with the calculated `iOver64Display`. See `nonceAndiOver64Compute`.
{-@ nonceAndiOver64Display :: { i:[_] | (len i) == 8 } -> _ -> { o:[_] | (len o) == 16 }  @-}
nonceAndiOver64Display :: [String] -> String -> [String]
nonceAndiOver64Display nonce index
    | length nonce == 8 = nonce ++ iOver64Display index
    | otherwise = error "first input to `nonceAndiOver64Display` must be a list of 8 `String` strings"

{-|
Retrieve a specific byte from the augmented key.
Given an augmented key, this function extracts the byte at the specified index and returns it as a `Word32`.
The augmented key is an array of 64 `Word32` values, and the index should be within the range [0, 63] to access
a valid byte.
-}
{-@ keybyteCompute :: { i:[_] | (len i) == 64 } -> {index: Nat | index <= 63 } -> _  @-}
keybyteCompute :: [Word32] -> Int -> Word32
keybyteCompute aumented_key index
    | length aumented_key == 64 = aumented_key!!index
    | otherwise = error "first input to `keybyteCompute` must be a list of 64 `Word32` numbers"

-- |Display a specific byte from the augmented key. See `keybyteCompute`.
{-@ keybyteDisplay :: { i:[_] | (len i) == 64 } -> {index: Nat | index <= 63 } -> _  @-}
keybyteDisplay :: [String] -> Int -> String
keybyteDisplay aumented_key index
    | length aumented_key == 64 = aumented_key!!index
    | otherwise = error "first input to `keybyteDisplay` must be a list of 64 `String` strings"

{-|
Generate the Salsa20 expansion matrix for a single 16-byte key.
This function computes the Salsa20 expansion matrix for a given 16-byte key and nonce, 
using an index to represent a message byte.
The resulting matrix is generated based on the provided key and nonce, where the key should be a list of
16 `Word32` values, and the nonce should be a list of 8 `Word32` values.
-}
{-@ crypt16Compute :: { key:[_] | (len key) == 16 } -> { nonce:[_] | (len nonce) == 8 } -> 
        Nat -> { o:[_] | (len o) == 64 }  @-}
crypt16Compute :: [Word32] -> [Word32] -> Int -> [Word32]
crypt16Compute key nonce index
    | length key == 16 && length nonce == 8 = expand16Compute key $ nonceAndiOver64Compute nonce index
    | otherwise = error "first input to `crypt16Compute` must be a list of 16 `Word32` numbers and the second a list of 8 `Word32` numbers"

{-|
Generate the Salsa20 expansion matrix as a list of strings for a single 16-byte key.
This function computes the Salsa20 expansion matrix as a list of strings for a given 16-byte key and nonce,
using an index to represent a message byte. The resulting matrix is generated based on the provided key and nonce,
where the key should be a list of 16 `String` values, and the nonce should be a list of 8 `String` values. 
The resulting matrix is returned as a list of strings.
-}
{-@ crypt16Display :: { key:[_] | (len key) == 16 } -> { nonce:[_] | (len nonce) == 8 } -> _ -> { o:[_] | (len o) == 64 }  @-}
crypt16Display :: [String] -> [String] -> String -> [String]
crypt16Display key nonce index
    | length key == 16 && length nonce == 8 = expand16Display key $ nonceAndiOver64Display nonce index
    | otherwise = error "first input to `crypt16Display` must be a list of 16 `String` strings and the second a list of 8 `String` strings"

{-|
Encrypt or decrypt a message using a single 16-byte key and return a encrypted/decrypted message of the same length.
It takes a list of `Word32` values of any length (message block), a 16-byte key, an 8-byte nonce, and an index representing
the position of the message byte.
The function performs XOR operations on the message block and the corresponding Salsa20 expansion matrix entry.
-}
{-@ cryptBlock16Compute :: _ -> { key:[_] | (len key) == 16 } -> { nonce:[_] | (len nonce) == 8 } -> Nat -> _  @-}
cryptBlock16Compute :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlock16Compute (x:xs) key nonce index
    | length key == 16 && length nonce == 8 = xor x (keybyteCompute (crypt16Compute key nonce index) (index `mod` 64)) :
        cryptBlock16Compute xs key nonce (index+1)
    | otherwise = error "first input to `cryptBlock16Compute` must be a list of 16 `Word32` numbers and the second a list of 8 `Word32` numbers"
cryptBlock16Compute _ _ _ _ = []

-- |Display the encryption or decryption of a message with a single 16 bytes key as a list of strings.
{-@ cryptBlock16Display :: _ -> { key:[_] | (len key) == 16 } -> { nonce:[_] | (len nonce) == 8 } -> Nat -> _  @-}
cryptBlock16Display :: [String] -> [String] -> [String] -> Int -> [String]
cryptBlock16Display (x:xs) key nonce index
    | length key == 16 && length nonce == 8 = printf "%s ⊕ %s" x (keybyteDisplay (crypt16Display key nonce (show index)) (index `mod` 64)) :
        cryptBlock16Display xs key nonce (index+1)
    | otherwise = error "first input to `cryptBlock16Display` must be a list of 16 `String` strings and the second a list of 8 `String` strings"
cryptBlock16Display _ _ _ _ = []

{-|
Generate the Salsa20 expansion matrix for a 32-byte key.
This function computes the Salsa20 expansion matrix for two given 16-byte keys and nonce, 
using an index to represent a message byte.
The resulting matrix is generated based on the provided key and nonce, where the key should be two lists of
16 `Word32` values, and the nonce should be a list of 8 `Word32` values.
-}
{-@ crypt32Compute :: { key1:[_] | (len key1) == 16 } -> {key2:[_] | (len key2) == 16 } -> 
        { nonce:[_] | (len nonce) == 8 } -> Nat -> { o:[_] | (len o) == 64 }  @-}
crypt32Compute :: [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
crypt32Compute key0 key1 nonce index 
    | length key0 == 16 && length key1 == 16 && length nonce == 8 = expand32Compute key0 key1 $ nonceAndiOver64Compute nonce index
    | otherwise = error "first input to `crypt32Compute` must be a list of 16 `Word32` numbers, the second a list of 16 `Word32` numbers and the third a list of 8 `Word32` numbers"

{-|
Generate the Salsa20 expansion matrix as a list of strings for a 32-byte key.
This function computes the Salsa20 expansion matrix as a list of strings for two given 16-byte key and nonce,
using an index to represent a message byte. The resulting matrix is generated based on the provided key and nonce,
where the key should be two lists of 16 `String` values, and the nonce should be a list of 8 `String` values. 
The resulting matrix is returned as a list of strings.
-}
{-@ crypt32Display :: { key1:[_] | (len key1) == 16 } -> {key2:[_] | (len key2) == 16 } -> 
        { nonce:[_] | (len nonce) == 8 } -> _ -> { o:[_] | (len o) == 64 }  @-}
crypt32Display :: [String] -> [String] -> [String] -> String -> [String]
crypt32Display key0 key1 nonce index
    | length key0 == 16 && length key1 == 16 && length nonce == 8 = expand32Display key0 key1 $ nonceAndiOver64Display nonce index
    | otherwise = error "first input to `crypt32Display` must be a list of 16 `String` strings, the second a list of 16 `String` strings and the third a list of 8 `String` strings"

{-|
Encrypt or decrypt a message using a 32-byte key and return a encrypted/decrypted message of the same length.
It takes a list of `Word32` values of any length (message block), two 16-byte keys, an 8-byte nonce, and an index representing
the position of the message byte.
The function performs XOR operations on the message block and the corresponding Salsa20 expansion matrix entry.
-}
{-@ cryptBlock32Compute :: _ -> { key1:[_] | (len key1) == 16 } -> {key2:[_] | (len key2) == 16 } -> 
        { nonce:[_] | (len nonce) == 8 } -> Nat -> _  @-}
cryptBlock32Compute :: [Word32] -> [Word32] -> [Word32] -> [Word32] -> Int -> [Word32]
cryptBlock32Compute (x:xs) key0 key1 nonce index
    | length key0 == 16 && length key1 == 16 && length nonce == 8 =
        xor x (keybyteCompute (crypt32Compute key0 key1 nonce index) (index `mod` 64)) :
            cryptBlock32Compute xs key0 key1 nonce (index+1)
    | otherwise = error "first input to `cryptBlock32Compute` must be a list of 16 `Word32` numbers, the second a list of 16 `Word32` numbers and the third a list of 8 `Word32` numbers"
cryptBlock32Compute _ _ _ _ _ = []

-- |Display the encryption or decryption of a message with a two 16 bytes key as a list of strings.
{-@ cryptBlock32Display :: _ -> { key1:[_] | (len key1) == 16 } -> {key2:[_] | (len key2) == 16 } -> 
        { nonce:[_] | (len nonce) == 8 } -> _ -> _  @-}
cryptBlock32Display :: [String] -> [String] -> [String] -> [String] -> Int -> [String]
cryptBlock32Display (x:xs) key0 key1 nonce index
    | length key0 == 16 && length key1 == 16 && length nonce == 8 =
        printf "%s ⊕ %s" x (keybyteDisplay (crypt32Display key0 key1 nonce (show index)) (index `mod` 64)) :
            cryptBlock32Display xs key0 key1 nonce (index+1)
    | otherwise = error "first input to `cryptBlock32Display` must be a list of 16 `String` strings, the second a list of 16 `String` strings and the third a list of 8 `String` strings"
cryptBlock32Display _ _ _ _ _ = []
