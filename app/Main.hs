{-|
Salsa20 Encryption and Decryption Application

This Haskell program demonstrates the encryption and decryption of messages using the Salsa20 stream cipher.
The application takes a user-provided secret key phrase and a message as input, performs encryption,
and then decrypts the message to demonstrate the Salsa20 algorithm.

This code uses:
- [hsalsa20 Crypt module](https://github.com/oxarbitrage/hsalsa20/blob/main/src/Crypt.hs) for encryption and decryption.
- [SHA256](https://hackage.haskell.org/package/cryptohash-sha256) for key hashing.
- [Crypto.Nonce](https://hackage.haskell.org/package/nonce) for nonce generation.
- [base64-bytestring](https://hackage.haskell.org/package/base64-bytestring) for encoding.

Based in https://asecuritysite.com/encryption/salsa20
-}
module Main (main) where

import Data.Word
import Data.Char (ord)
import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.Nonce
import Data.ByteString.Base64 as B64
import Data.List.Split

import Crypt

{-@ ignore main @-}
main :: IO ()
main = do
    printInfo ["---Salsa20 encryption and decryption", ""]

    key_string <- getInput "Insert your secret key phrase:"
    message_string <- getInput "Insert message to be encrypted or decrypted:"

    printInfo ["", "Plain text: " ++ message_string, "Secret key: " ++ key_string]

    -- Hash the key
    let key_hash = keyHash key_string
    -- Encode the key as base64
    let key_base64 = toBase64 key_hash

    printInfo ["Key used: " ++ byteStringtoCharList key_base64]

    printInfo ["", "---Salsa20 Encrypt"]

    -- Nonce
    generated_nonce <- generateNonce
    printInfo ["Nonce: " ++ byteStringtoCharList (toBase64 generated_nonce)]

    -- Split the hashed key into two keys of 16 bytes each
    let (key1, key2) = splitKey key_hash

    -- Convert nonce to a list of `[Word32]` that we can use as input in `cryptBlock32Compute`
    let nonce = word8ListToWord32List generated_nonce

    -- Convert the mesage to a list of `[Word32]` that we can use as input in `cryptBlock32Compute`
    --let message = intToWord32 $ stringToBytes message_string
    let message = stringToWord32List message_string

    -- Call encryption function.
    let v2encrypted = cryptBlock32Compute message key1 key2 nonce 0

    -- Encode result 
    let b64_cipher = toBase64 (word32ListToWord8List v2encrypted)
    printInfo ["Ciphertext: " ++ byteStringtoCharList b64_cipher, "", "---Salsa20 Decrypt"]

    -- Decrypt
    let v2decrypted = cryptBlock32Compute v2encrypted key1 key2 nonce 0
    let b64_decrypted = word32ListToByteString v2decrypted
    putStrLn ("Decrypted: " ++ byteStringtoCharList b64_decrypted)

    return ()

getInput :: String -> IO String
getInput prompt = do
    putStrLn prompt
    getLine

printInfo :: [String] -> IO ()
printInfo = mapM_ putStrLn

-- | Converts a string to a ByteString using ASCII encoding.
stringToByteString :: String -> B.ByteString
stringToByteString = B.pack . map (fromIntegral . ord)

-- | Converts a string to a list of Word32.
stringToWord32List :: String -> [Word32]
stringToWord32List = map fromIntegral . map ord

-- | Converts a list of Word32 back to a ByteString.
word32ListToByteString :: [Word32] -> B.ByteString
word32ListToByteString = B.pack . map fromIntegral

-- | Converts a ByteString to a human-readable character list.
byteStringtoCharList :: B.ByteString -> [Char]
byteStringtoCharList = map (toEnum . fromEnum) . B.unpack

-- | Converts a list of Word8 to a list of Word32.
word8ListToWord32List :: [Word8] -> [Word32]
word8ListToWord32List = map fromIntegral

-- | Converts a list of Word32 back to a list of Word8.
word32ListToWord8List :: [Word32] -> [Word8]
word32ListToWord8List = map fromIntegral

-- | Hashes a string using SHA256 and returns a list of Word8.
keyHash :: String -> [Word8]
keyHash key = B.unpack $ SHA256.hash (stringToByteString key)

-- | Encodes a list of Word8 into a Base64 ByteString.
toBase64 :: [Word8] -> B.ByteString
toBase64 bytes = B64.encode $ B.pack bytes

-- | Generates a cryptographic nonce.
{-@ ignore generateNonce @-}
generateNonce :: IO [Word8]
generateNonce = do
    -- Generate nonce
    g <- Crypto.Nonce.new
    nonce <- Crypto.Nonce.nonce128 g

    let unpackedNonce = B.unpack nonce
    -- We just need half of the nonce so we split it in two and take the first half
    let nonces = Data.List.Split.chunksOf 8 unpackedNonce
    return $ head nonces

-- | Splits a hashed key into two separate keys, each being a list of Word32.
splitKey :: [Word8] -> ([Word32], [Word32])
splitKey key_hash = do
    let keys = Data.List.Split.chunksOf 16 key_hash
    (word8ListToWord32List $ keys!!0, word8ListToWord32List $ keys!!1)
