import Data.Word

import Crypt
import Vectors

import Text.Hex (decodeHex)
import qualified Data.Text as T
import qualified Data.ByteString as ByteString
import Data.Maybe (fromJust)

-- Given a valid hex string, convert to a list of numbers. Panic if string is not valid hex.
encodeHexString :: String -> [Word32]
encodeHexString key = map fromIntegral $ ByteString.unpack $ fromJust $ decodeHex $ T.pack key

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

    let set1_vector9_output = cryptBlockV2 message (encodeHexString set1_vector9_key1)
            (encodeHexString set1_vector9_key2) (encodeHexString set1_vector9_iv) 0

    putStrLn "Set1, vector #9:"

    putStrLn $ if take 64 set1_vector9_output == encodeHexString set1_vector9_expected_output_0_63
        then "OK" else "FAIL!"
    putStrLn $ if drop 192 (take 256 set1_vector9_output) == encodeHexString set1_vector9_expected_output_192_255
        then "OK" else "FAIL!"
    putStrLn $ if drop 256 (take 320 set1_vector9_output) == encodeHexString set1_vector9_expected_output_256_319
        then "OK" else "FAIL!"
    putStrLn $ if drop 448 set1_vector9_output == encodeHexString set1_vector9_expected_output_448_511
        then "OK" else "FAIL!"

    putStrLn ""

    let set1_vector18_output = cryptBlockV2 message (encodeHexString set1_vector18_key1)
            (encodeHexString set1_vector18_key2) (encodeHexString set1_vector18_iv) 0

    putStrLn "Set1, vector #18:"

    putStrLn $ if take 64 set1_vector18_output == encodeHexString set1_vector18_expected_output_0_63
        then "OK" else "FAIL!"
    putStrLn $ if drop 192 (take 256 set1_vector18_output) == encodeHexString set1_vector18_expected_output_192_255
        then "OK" else "FAIL!"
    putStrLn $ if drop 256 (take 320 set1_vector18_output) == encodeHexString set1_vector18_expected_output_256_319
        then "OK" else "FAIL!"
    putStrLn $ if drop 448 set1_vector18_output == encodeHexString set1_vector18_expected_output_448_511
        then "OK" else "FAIL!"

    putStrLn ""

    let set1_vector27_output = cryptBlockV2 message (encodeHexString set1_vector27_key1)
            (encodeHexString set1_vector27_key2) (encodeHexString set1_vector27_iv) 0

    putStrLn "Set1, vector #27:"

    putStrLn $ if take 64 set1_vector27_output == encodeHexString set1_vector27_expected_output_0_63
        then "OK" else "FAIL!"
    putStrLn $ if drop 192 (take 256 set1_vector27_output) == encodeHexString set1_vector27_expected_output_192_255
        then "OK" else "FAIL!"
    putStrLn $ if drop 256 (take 320 set1_vector27_output) == encodeHexString set1_vector27_expected_output_256_319
        then "OK" else "FAIL!"
    putStrLn $ if drop 448 set1_vector27_output == encodeHexString set1_vector27_expected_output_448_511
        then "OK" else "FAIL!"

    putStrLn ""

    let set1_vector36_output = cryptBlockV2 message (encodeHexString set1_vector36_key1)
            (encodeHexString set1_vector36_key2) (encodeHexString set1_vector36_iv) 0

    putStrLn "Set1, vector #36:"

    putStrLn $ if take 64 set1_vector36_output == encodeHexString set1_vector36_expected_output_0_63
        then "OK" else "FAIL!"
    putStrLn $ if drop 192 (take 256 set1_vector36_output) == encodeHexString set1_vector36_expected_output_192_255
        then "OK" else "FAIL!"
    putStrLn $ if drop 256 (take 320 set1_vector36_output) == encodeHexString set1_vector36_expected_output_256_319
        then "OK" else "FAIL!"
    putStrLn $ if drop 448 set1_vector36_output == encodeHexString set1_vector36_expected_output_448_511
        then "OK" else "FAIL!"

    putStrLn ""


    return ()
