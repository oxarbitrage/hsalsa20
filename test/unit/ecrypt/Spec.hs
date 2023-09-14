import Data.Word

import Crypt

import Text.Hex (decodeHex)
import qualified Data.Text as T
import qualified Data.ByteString as ByteString
import Data.Maybe (fromJust)

-- Given a valid hex string, convert to a list of numbers. Panic if string is not valid hex.
encodeHexString :: String -> [Word32]
encodeHexString key = map fromIntegral $ ByteString.unpack $ fromJust $ decodeHex $ T.pack key

-- Set 1, vector#  0:

set1_vector0_key1 :: String
set1_vector0_key1 = "80000000000000000000000000000000"

set1_vector0_key2 :: String
set1_vector0_key2 = "00000000000000000000000000000000"

set1_vector0_iv :: String
set1_vector0_iv = "0000000000000000"

set1_vector0_expected_output_0_63 :: String
set1_vector0_expected_output_0_63 = "E3BE8FDD8BECA2E3EA8EF9475B29A6E7\
    \003951E1097A5C38D23B7A5FAD9F6844\
    \B22C97559E2723C7CBBD3FE4FC8D9A07\
    \44652A83E72A9C461876AF4D7EF1A117"

set1_vector0_expected_output_192_255 :: String
set1_vector0_expected_output_192_255 = "57BE81F47B17D9AE7C4FF15429A73E10\
    \ACF250ED3A90A93C711308A74C6216A9\
    \ED84CD126DA7F28E8ABF8BB63517E1CA\
    \98E712F4FB2E1A6AED9FDC73291FAA17"

set1_vector0_expected_output_256_319 :: String
set1_vector0_expected_output_256_319 = "958211C4BA2EBD5838C635EDB81F513A\
    \91A294E194F1C039AEEC657DCE40AA7E\
    \7C0AF57CACEFA40C9F14B71A4B3456A6\
    \3E162EC7D8D10B8FFB1810D71001B618"

set1_vector0_expected_output_448_511 :: String
set1_vector0_expected_output_448_511 = "696AFCFD0CDDCC83C7E77F11A649D79A\
    \CDC3354E9635FF137E929933A0BD6F53\
    \77EFA105A3A4266B7C0D089D08F1E855\
    \CC32B15B93784A36E56A76CC64BC8477"

message :: [Word32]
message = replicate 512 0

main :: IO ()
main = do
    putStrLn "-- Salsa20 ECRYPT test vectors --"
    putStrLn ""

    let set1_vector0_output = cryptBlockV2 message (encodeHexString set1_vector0_key1) 
            (encodeHexString set1_vector0_key2) (encodeHexString set1_vector0_iv) 0

    putStrLn "Set1, vector #0:"

    putStrLn $ if take 64 set1_vector0_output == encodeHexString set1_vector0_expected_output_0_63 
        then "OK" else "FAIL!"
    putStrLn $ if drop 192 (take 256 set1_vector0_output) == encodeHexString set1_vector0_expected_output_192_255 
        then "OK" else "FAIL!"
    putStrLn $ if drop 256 (take 320 set1_vector0_output) == encodeHexString set1_vector0_expected_output_256_319
        then "OK" else "FAIL!"
    putStrLn $ if drop 448 set1_vector0_output == encodeHexString set1_vector0_expected_output_448_511
        then "OK" else "FAIL!"

    putStrLn ""

    return ()
