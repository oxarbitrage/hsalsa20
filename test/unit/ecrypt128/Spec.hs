{-|
ECRYPT tests for the 128 bit key size.

TODO: 
- Add the xordigest tests.
- This file is almost the same code as `ecrypt256/Spec.hs`. We should refactor it.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Word

import Crypt

import Text.Hex (decodeHex)
import qualified Data.Text as T
import qualified Data.ByteString as ByteString
import Data.Maybe (fromJust, fromMaybe)
import Data.Map

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Control.Monad (forM_)

-- The test data from the json
data TestVector = TestVector 
    { key1 :: String
    , iv :: String
    , stream1index  :: Int
    , stream1expected :: String
    , stream2index  :: Int
    , stream2expected :: String
    , stream3index  :: Int
    , stream3expected :: String
    , stream4index  :: Int
    , stream4expected :: String
    , xordigest :: String
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
data CollectedTestData = CollectedTestData 
    { test_name_list :: [String]
    , key1_list :: [String]
    , iv_list :: [String]
    , stream1index_list  :: [Int]
    , stream1expected_list :: [String]
    , stream2index_list  :: [Int]
    , stream2expected_list :: [String]
    , stream3index_list  :: [Int]
    , stream3expected_list :: [String]
    , stream4index_list  :: [Int]
    , stream4expected_list :: [String]
    , xordigest_list :: [String]
} deriving (Generic, Show)

-- Collect data from the JSON string, prepare it, and return a CollectedTestData
collect :: B.ByteString -> CollectedTestData
collect json_file = CollectedTestData
    { test_name_list = concatMap gettestname decoded
    , key1_list = cleanupField key1
    , iv_list = cleanupField iv
    , stream1index_list = getFieldValues stream1index
    , stream1expected_list = cleanupField stream1expected
    , stream2index_list = getFieldValues stream2index
    , stream2expected_list = cleanupField stream2expected
    , stream3index_list = getFieldValues stream3index
    , stream3expected_list = cleanupField stream3expected
    , stream4index_list = getFieldValues stream4index
    , stream4expected_list = cleanupField stream4expected
    , xordigest_list = []
    }
  where
    -- Decode the JSON and handle the Maybe
    decoded :: [Map String TestVector]
    decoded = fromMaybe [] $ decode json_file

    -- Helper function to get field values
    getFieldValues :: (TestVector -> a) -> [a]
    getFieldValues field = concatMap (getelementlist . getField field) decoded

    -- Helper function to clean up field values
    cleanupField :: (TestVector -> String) -> [String]
    cleanupField field = cleanup $ concatMap (getelementlist . getField field) decoded

-- Process all test vectors
processTestVectors :: [Word32] -> CollectedTestData -> IO ()
processTestVectors message collected = do
    let zipped = zip (test_name_list collected) [0..]

    forM_ zipped $ \(testName, idx) -> do
        let runTest streamIndex getExpected = do
                let output = cryptBlockV1 message
                        (encodeHexString (key1_list collected !! idx))
                        (encodeHexString (iv_list collected !! idx))
                        (streamIndex !! idx)

                let expectedOutput = encodeHexString (getExpected collected !! idx)
                putStrLn $ if output == expectedOutput then "OK" else "FAIL!"

        putStrLn testName
        runTest (stream1index_list collected) stream1expected_list
        runTest (stream2index_list collected) stream2expected_list
        runTest (stream3index_list collected) stream3expected_list
        runTest (stream4index_list collected) stream4expected_list

main :: IO ()
main = do
    putStrLn "-- Salsa20 ECRYPT 128 bit key test vectors --"
    putStrLn ""

    -- Read the 128 bit key size json.
    json_file <- B.readFile "test/unit/ecrypt128/test_vectors_128.json"

    -- Collect data as lists.
    let collected = collect json_file

    -- Run tests with our collected data.
    let message = replicate 64 0 :: [Word32]
    processTestVectors message collected

    return ()
