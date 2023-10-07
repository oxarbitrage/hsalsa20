{-# LANGUAGE DataKinds #-}

import Quarterround
import Rowround
import Columnround

import Test.HUnit
import Keelung

import Data.Word

rowroundInputUInt :: [UInt 32]
rowroundInputUInt = [
    0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
    0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
    0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
    0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]

rowroundInputWord32 :: [Word32]
rowroundInputWord32 = [
    0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
    0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
    0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
    0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]

main :: IO Counts
main = do
    -- Run tests

    putStrLn "Running Keelung quarterround tests:"
    putStrLn ""

    let quarterround_computed = quarterroundCompute [1, 0, 0, 0]
    putStrLn "Quarterround computed for input [1, 0, 0, 0]:"
    print quarterround_computed

    quarterround_interpreted <- interpret gf181 (quarterroundKeelung [1, 0, 0, 0]) [] []
    putStrLn "Quarterround simulated for input [1, 0, 0, 0]:"
    print quarterround_interpreted

    let quarterround_computed2 = quarterroundCompute [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]
    putStrLn "Quarterround computed for input [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]:"
    print quarterround_computed2

    quarterround_interpreted2 <- interpret gf181 (quarterroundKeelung [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]) [] []
    putStrLn "Quarterround simulated for input [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]:"
    print quarterround_interpreted2

    _quarterround_compiled <- compile bn128 (quarterroundKeelung [1, 0, 0, 0])
    --putStrLn "Quarterround compiled:"
    --print quarterround_compiled

    let rowround_computed = rowroundCompute rowroundInputWord32
    putStrLn "Rowround computed for input rowroundInputWord32:"
    print rowround_computed

    rowround_interpreted2 <- interpret gf181 (rowroundKeelung rowroundInputUInt) [] []
    putStrLn "Rowround simulated for input rowroundInputUInt:"
    print rowround_interpreted2

    let columnround_computed = columnroundCompute rowroundInputWord32
    putStrLn "Columnround computed for input rowroundInputWord32:"
    print columnround_computed

    columnround_interpreted2 <- interpret gf181 (columnroundKeelung rowroundInputUInt) [] []
    putStrLn "Columnround simulated for input rowroundInputUInt:"
    print columnround_interpreted2

    -- just return an empty `Count` so we don't have to return the one from a specific test:
    return (Counts 0 0 0 0)
