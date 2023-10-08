{-# LANGUAGE DataKinds #-}

import Quarterround
import Rowround
import Columnround
import Doubleround

import Test.HUnit
import Keelung

import Data.Word
import Data.Either (fromRight)
import Keelung.Constraint.R1CS (toR1Cs)

demoInputUInt32 :: [UInt 32]
demoInputUInt32 = [
    0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
    0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
    0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
    0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]

demoInputWord32 :: [Word32]
demoInputWord32 = [
    0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
    0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
    0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
    0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]

main :: IO Counts
main = do
    -- Run tests

    let quarterround_computed = quarterroundCompute [1, 0, 0, 0]
    quarterround_interpreted <- interpret gf181 (quarterroundKeelung [1, 0, 0, 0]) [] []
    putStrLn $ if quarterround_computed == map fromIntegral quarterround_interpreted then "OK" else "FAIL!"

    quarterround_compiled <- compile bn128 (quarterroundKeelung [1, 0, 0, 0])
    let quarterround_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") quarterround_compiled)
    putStrLn $ if quarterround_constraints == 2393 then "OK" else "FAIL!"

    let quarterround_computed2 = quarterroundCompute [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]
    quarterround_interpreted2 <- interpret gf181 (quarterroundKeelung [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]) [] []
    putStrLn $ if quarterround_computed2 == map fromIntegral quarterround_interpreted2 then "OK" else "FAIL!"

    quarterround_compiled2 <- compile bn128 (quarterroundKeelung [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137])
    let quarterround_constraints2 = length $ toR1Cs (fromRight (error "error parsing r1cs") quarterround_compiled2)
    putStrLn $ if quarterround_constraints2 == 2393 then "OK" else "FAIL!"

    let rowround_computed = rowroundCompute demoInputWord32
    rowround_interpreted <- interpret gf181 (rowroundKeelung demoInputUInt32) [] []
    putStrLn $ if rowround_computed == map fromIntegral rowround_interpreted then "OK" else "FAIL!"

    rowround_compiled <- compile bn128 (rowroundKeelung demoInputUInt32)
    let rowround_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") rowround_compiled)
    putStrLn $ if rowround_constraints == 9572 then "OK" else "FAIL!"

    let columnround_computed = columnroundCompute demoInputWord32
    columnround_interpreted <- interpret gf181 (columnroundKeelung demoInputUInt32) [] []
    putStrLn $ if columnround_computed == map fromIntegral columnround_interpreted then "OK" else "FAIL!"

    columnround_compiled <- compile bn128 (columnroundKeelung demoInputUInt32)
    let columnround_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") columnround_compiled)
    putStrLn $ if columnround_constraints == 9572 then "OK" else "FAIL!"

    let doubleround_computed = doubleroundCompute demoInputWord32
    doubleround_interpreted <- interpret gf181 (doubleroundKeelung demoInputUInt32) [] []
    putStrLn $ if doubleround_computed == map fromIntegral doubleround_interpreted then "OK" else "FAIL!"

    doubleround_compiled <- compile bn128 (doubleroundKeelung demoInputUInt32)
    let douleround_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") doubleround_compiled)
    putStrLn $ if douleround_constraints == 18644 then "OK" else "FAIL!"

    let doubleroundR_computed = doubleroundRCompute demoInputWord32 2
    doubleroundR_interpreted <- interpret gf181 (doubleroundRKeelung demoInputUInt32 2) [] []
    putStrLn $ if doubleroundR_computed == map fromIntegral doubleroundR_interpreted then "OK" else "FAIL!"

    doubleroundR_compiled <- compile bn128 (doubleroundRKeelung demoInputUInt32 2)
    let douleroundR_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") doubleroundR_compiled)
    putStrLn $ if douleroundR_constraints == 36788 then "OK" else "FAIL!"

    let doubleround10_computed = doubleroundRCompute demoInputWord32 10
    doubleround10_interpreted <- interpret gf181 (doubleroundRKeelung demoInputUInt32 10) [] []
    putStrLn $ if doubleround10_computed == map fromIntegral doubleround10_interpreted then "OK" else "FAIL!"

    doubleround10_compiled <- compile bn128 (doubleroundRKeelung demoInputUInt32 10)
    let douleround10_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") doubleround10_compiled)
    putStrLn $ if douleround10_constraints == 181940 then "OK" else "FAIL!"

    -- just return an empty `Count` so we don't have to return the one from a specific test:
    return (Counts 0 0 0 0)
