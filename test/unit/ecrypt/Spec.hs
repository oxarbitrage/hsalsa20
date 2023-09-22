{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Word

import Crypt

import Text.Hex (decodeHex)
import qualified Data.Text as T
import qualified Data.ByteString as ByteString
import Data.Maybe (fromJust)
import Data.Map

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- The test data
data TestVector = TestVector {
    key1 :: String,
    key2 :: String,
    iv :: String,
    stream1index  :: Int,
    stream1expected :: String,
    stream2index  :: Int,
    stream2expected :: String,
    stream3index  :: Int,
    stream3expected :: String,
    stream4index  :: Int,
    stream4expected :: String,
    xordigest :: String
} deriving (Generic, Show)

-- No need to provide a parseJSON implementation.
instance FromJSON TestVector

-- Get all test name as a list
gettestname :: Map k a -> [k]
gettestname = Data.Map.keys

-- Get all `key1` as a list.
getkey1 :: Map k TestVector -> Map k String
getkey1 = Data.Map.map (show . key1)

-- Get all `key2` as a list.
getkey2 :: Map k TestVector -> Map k String
getkey2 = Data.Map.map (show . key2)

-- Get all `iv` as a list.
getiv :: Map k TestVector -> Map k String
getiv = Data.Map.map (show . iv)

-- Get all `stream1_index` as a list.
getstream1_index :: Map k TestVector -> Map k String
getstream1_index = Data.Map.map (show . stream1index)

-- Get all `stream1_expected` as a list.
getstream1_expected :: Map k TestVector -> Map k String
getstream1_expected = Data.Map.map (show . stream1expected)

-- Get all `stream2_index` as a list.
getstream2_index :: Map k TestVector -> Map k String
getstream2_index = Data.Map.map (show . stream2index)

-- Get all `stream2_expected` as a list.
getstream2_expected :: Map k TestVector -> Map k String
getstream2_expected = Data.Map.map (show . stream2expected)

-- Get all `stream3_index` as a list.
getstream3_index :: Map k TestVector -> Map k String
getstream3_index = Data.Map.map (show . stream3index)

-- Get all `stream3_expected` as a list.
getstream3_expected :: Map k TestVector -> Map k String
getstream3_expected = Data.Map.map (show . stream3expected)

-- Get all `stream4_index` as a list.
getstream4_index :: Map k TestVector -> Map k String
getstream4_index = Data.Map.map (show . stream4index)

-- Get all `stream4_expected` as a list.
getstream4_expected :: Map k TestVector -> Map k String
getstream4_expected = Data.Map.map (show . stream4expected)

-- Given a map of key values get a list of values.
getelementlist :: Map k a -> [a]
getelementlist = Data.Map.elems

-- After `readfile` reads the json file it will add additional double quotes that we clean here.
cleanup :: [[Char]] -> [[Char]]
cleanup = Prelude.map (Prelude.filter (/='"'))

-- Given a valid hex string, convert to a list of numbers. Panic if string is not valid hex.
encodeHexString :: String -> [Word32]
encodeHexString key = Prelude.map fromIntegral $ ByteString.unpack $ fromJust $ decodeHex $ T.pack key

-- Collect data from the json string, returning a list of lists with each with test data.
collect :: B.ByteString -> [[String]]
collect json_file = do
    let maybe_decoded = decode json_file :: Maybe [Map String TestVector]
    let decoded = fromJust maybe_decoded

    let test_name_list = concatMap gettestname decoded
    let key1_list = cleanup $ concatMap (getelementlist . getkey1) decoded
    let key2_list = cleanup $ concatMap (getelementlist . getkey2) decoded
    let iv_list = cleanup $ concatMap (getelementlist . getiv) decoded

    let stream1_index_list = concatMap (getelementlist . getstream1_index) decoded
    let stream1_expected_list = cleanup $ concatMap (getelementlist . getstream1_expected) decoded

    let stream2_index_list = concatMap (getelementlist . getstream2_index) decoded
    let stream2_expected_list = cleanup $ concatMap (getelementlist . getstream2_expected) decoded

    let stream3_index_list = concatMap (getelementlist . getstream3_index) decoded
    let stream3_expected_list = cleanup $ concatMap (getelementlist . getstream3_expected) decoded

    let stream4_index_list = concatMap (getelementlist . getstream4_index) decoded
    let stream4_expected_list = cleanup $ concatMap (getelementlist . getstream4_expected) decoded

    [
        test_name_list, key1_list, key2_list, iv_list, stream1_index_list, stream1_expected_list,
        stream2_index_list, stream2_expected_list, stream3_index_list, stream3_expected_list,
        stream4_index_list, stream4_expected_list]


-- Run the test given all the 12 lists of data.
run :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] ->
    [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> IO ()
run (test_name_x:test_name_xs) (key1_x:key1_xs) (key2_x:key2_xs) 
    (iv_x:iv_xs) (stream1_index_x:stream1_index_xs) (stream1_expected_x:stream1_expected_xs) 
    (stream2_index_x:stream2_index_xs) (stream2_expected_x:stream2_expected_xs)
    (stream3_index_x:stream3_index_xs) (stream3_expected_x:stream3_expected_xs)
    (stream4_index_x:stream4_index_xs) (stream4_expected_x:stream4_expected_xs) = do

    putStrLn test_name_x

    let message = Prelude.replicate 64 0 :: [Word32]

    let stream1_output = cryptBlockV2 message (encodeHexString key1_x)
            (encodeHexString key2_x) (encodeHexString iv_x) (read stream1_index_x)
    
    putStrLn $ if stream1_output == encodeHexString stream1_expected_x then "OK" else "FAIL!"

    let stream2_output = cryptBlockV2 message (encodeHexString key1_x)
            (encodeHexString key2_x) (encodeHexString iv_x) (read stream2_index_x)

    putStrLn $ if stream2_output == encodeHexString stream2_expected_x then "OK" else "FAIL!"

    let stream3_output = cryptBlockV2 message (encodeHexString key1_x)
            (encodeHexString key2_x) (encodeHexString iv_x) (read stream3_index_x)

    putStrLn $ if stream3_output == encodeHexString stream3_expected_x then "OK" else "FAIL!"

    let stream4_output = cryptBlockV2 message (encodeHexString key1_x)
            (encodeHexString key2_x) (encodeHexString iv_x) (read stream4_index_x)

    putStrLn $ if stream4_output == encodeHexString stream4_expected_x then "OK" else "FAIL!"

    run test_name_xs key1_xs key2_xs iv_xs stream1_index_xs stream1_expected_xs
        stream2_index_xs stream2_expected_xs stream3_index_xs stream3_expected_xs
        stream4_index_xs stream4_expected_xs
run _ _ _ _ _ _ _ _ _ _ _ _ = return ()

main :: IO ()
main = do
    putStrLn "-- Salsa20 ECRYPT test vectors --"
    putStrLn ""

    -- Read the 256 bit key size json.
    json_file <- B.readFile "test/unit/ecrypt/test_vectors_256.json"

    -- Collect data as lists.
    let collected = collect json_file

    -- Run tests with our collected lists.
    run (head collected) (collected!!1) (collected!!2) (collected!!3) (collected!!4) (collected!!5) 
        (collected!!6) (collected!!7) (collected!!8) (collected!!9) (collected!!10) (collected!!11)

    return ()
