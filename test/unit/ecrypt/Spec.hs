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

-- The test data from the json
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

-- Get a field of the test vector.
getField :: (TestVector -> a) -> Map k TestVector -> Map k a
getField = Data.Map.map

-- Given a map of key values get a list of values.
getelementlist :: Map k a -> [a]
getelementlist = Data.Map.elems

-- After `readfile` reads the json file it will add additional double quotes that we clean here.
cleanup :: [[Char]] -> [[Char]]
cleanup = Prelude.map (Prelude.filter (/='"'))

-- Given a valid hex string, convert to a list of numbers. Panic if string is not valid hex.
encodeHexString :: String -> [Word32]
encodeHexString key = Prelude.map fromIntegral $ ByteString.unpack $ fromJust $ decodeHex $ T.pack key

-- The prepared, generated data from the json as lists.
data CollectedTestData = CollectedTestData {
    test_name_list :: [String],
    key1_list :: [String],
    key2_list :: [String],
    iv_list :: [String],
    stream1index_list  :: [Int],
    stream1expected_list :: [String],
    stream2index_list  :: [Int],
    stream2expected_list :: [String],
    stream3index_list  :: [Int],
    stream3expected_list :: [String],
    stream4index_list  :: [Int],
    stream4expected_list :: [String],
    xordigest_list :: [String]
} deriving (Generic, Show)

-- Collect data from the json string, prepare it and return a list of lists with each test data field.
collect :: B.ByteString -> CollectedTestData
collect json_file = do
    let maybe_decoded = decode json_file :: Maybe [Map String TestVector]
    let decoded = fromJust maybe_decoded

    let test_name_list = concatMap gettestname decoded
    let key1_list = cleanup $ concatMap (getelementlist . getField key1) decoded
    let key2_list = cleanup $ concatMap (getelementlist . getField key2) decoded
    let iv_list = cleanup $ concatMap (getelementlist . getField iv) decoded

    let stream1index_list = concatMap (getelementlist . getField stream1index) decoded
    let stream1expected_list = cleanup $ concatMap (getelementlist . getField stream1expected) decoded

    let stream2index_list = concatMap (getelementlist . getField stream2index) decoded
    let stream2expected_list = cleanup $ concatMap (getelementlist . getField stream2expected) decoded

    let stream3index_list = concatMap (getelementlist . getField stream3index) decoded
    let stream3expected_list = cleanup $ concatMap (getelementlist . getField stream3expected) decoded

    let stream4index_list = concatMap (getelementlist . getField stream4index) decoded
    let stream4expected_list = cleanup $ concatMap (getelementlist . getField stream4expected) decoded

    let xordigest_list = []

    CollectedTestData test_name_list key1_list key2_list iv_list stream1index_list stream1expected_list
        stream2index_list stream2expected_list stream3index_list stream3expected_list
        stream4index_list stream4expected_list xordigest_list

-- Run the test given the prepared collected data from the json file.
run :: CollectedTestData -> IO()
run c = do
    putStrLn (head (test_name_list c))

    let message = Prelude.replicate 64 0 :: [Word32]

    let stream1_output = cryptBlockV2 message (encodeHexString (head (key1_list c)))
            (encodeHexString (head (key2_list c))) (encodeHexString (head (iv_list c))) (head (stream1index_list c))
    
    putStrLn $ if stream1_output == encodeHexString (head (stream1expected_list c)) then "OK" else "FAIL!"

    let stream2_output = cryptBlockV2 message (encodeHexString (head (key1_list c)))
            (encodeHexString (head (key2_list c))) (encodeHexString (head (iv_list c))) (head (stream2index_list c))

    putStrLn $ if stream2_output == encodeHexString (head (stream2expected_list c)) then "OK" else "FAIL!"

    let stream3_output = cryptBlockV2 message (encodeHexString (head (key1_list c)))
            (encodeHexString (head (key2_list c))) (encodeHexString (head (iv_list c))) (head (stream3index_list c))

    putStrLn $ if stream3_output == encodeHexString (head (stream3expected_list c)) then "OK" else "FAIL!"

    let stream4_output = cryptBlockV2 message (encodeHexString (head (key1_list c)))
            (encodeHexString (head (key2_list c))) (encodeHexString (head (iv_list c))) (head (stream4index_list c))

    putStrLn $ if stream4_output == encodeHexString (head (stream4expected_list c)) then "OK" else "FAIL!"

    -- TODO: maybe do xor-digest field
    let xordigest_dummy = []

    run (CollectedTestData (tail $ test_name_list c) (tail $ key1_list c) (tail $ key2_list c) (tail $ iv_list c)
        (tail $ stream1index_list c) (tail $ stream1expected_list c)
        (tail $ stream2index_list c) (tail $ stream2expected_list c)
        (tail $ stream3index_list c) (tail $ stream3expected_list c)
        (tail $ stream4index_list c) (tail $ stream4expected_list c) xordigest_dummy)

main :: IO ()
main = do
    putStrLn "-- Salsa20 ECRYPT test vectors --"
    putStrLn ""

    -- Read the 256 bit key size json.
    json_file <- B.readFile "test/unit/ecrypt/test_vectors_256.json"

    -- Collect data as lists.
    let collected = collect json_file

    -- Run tests with our collected lists.
    run collected

    return ()
