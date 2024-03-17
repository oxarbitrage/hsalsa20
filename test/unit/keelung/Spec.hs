{- Keelung tests.

`stack test salsa20:test:salsa20-keelung-unit-tests`

or

`cabal test salsa20:test:salsa20-keelung-unit-tests`

Need `keelung-compiler` version `v0.19.1` installed in the local machine.
-}
{-# LANGUAGE DataKinds #-}

import Quarterround
import Rowround
import Columnround
import Doubleround
import Hash

import Test.HUnit
import Keelung

import Data.Word
import Data.Either (fromRight)
import Keelung.Constraint.R1CS (toR1Cs)

demoInputWord32 :: [Word32]
demoInputWord32 = [
    0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
    0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
    0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
    0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]

demoInputUInt32 :: [UInt 32]
demoInputUInt32 = map fromIntegral demoInputWord32

demoSalsa20InputWord32 :: [Word32]
demoSalsa20InputWord32 = [
    88, 118, 104, 54,
    79, 201, 235, 79,
    3, 81, 156, 47,
    203, 26, 244, 243,
    191, 187, 234, 136,
    211, 159, 13, 115,
    76, 55, 82, 183,
    3, 117, 222, 37,
    86, 16, 179, 207,
    49, 237, 179, 48,
    1, 106, 178, 219,
    175, 199, 166, 48,
    238, 55, 204, 36,
    31, 240, 32, 63,
    15, 83, 93, 161,
    116, 147, 48, 113]

demoSalsa20InputUInt32 :: [UInt 32]
demoSalsa20InputUInt32 = map fromIntegral demoSalsa20InputWord32

{-@ ignore main @-}
main :: IO Counts
main = do
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

    doubleround2_compiled <- compile bn128 (doubleroundRKeelung 2 demoInputUInt32)
    let douleround2_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") doubleround2_compiled)
    putStrLn $ if douleround2_constraints == 36788 then "OK" else "FAIL!"

    let doubleround2_computed = doubleroundRCompute 2 demoInputWord32
    doubleround2_interpreted <- interpret gf181 (doubleroundRKeelung 2 demoInputUInt32) [] []
    putStrLn $ if doubleround2_computed == map fromIntegral doubleround2_interpreted then "OK" else "FAIL!"

    doubleround10_compiled <- compile bn128 (doubleroundRKeelung 10 demoInputUInt32)
    let douleround10_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") doubleround10_compiled)
    putStrLn $ if douleround10_constraints == 181940 then "OK" else "FAIL!"

    let doubleround10_computed = doubleroundRCompute 10 demoInputWord32
    doubleround10_interpreted <- interpret gf181 (doubleroundRKeelung 10 demoInputUInt32) [] []
    putStrLn $ if doubleround10_computed == map fromIntegral doubleround10_interpreted then "OK" else "FAIL!"

    let core_computed = coreCompute 2 demoInputWord32
    core_interpreted <- interpret gf181 (coreKeelung 2 demoInputUInt32) [] []
    putStrLn $ if core_computed == map fromIntegral core_interpreted then "OK" else "FAIL!"

    core_compiled <- compile bn128 (coreKeelung 2 demoInputUInt32)
    let core_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") core_compiled)
    putStrLn $ if core_constraints == 36804 then "OK" else "FAIL!"

    let salsa20_4_computed = salsa20Compute 2 demoSalsa20InputWord32
    salsa20_4_interpreted <- interpret gf181 (salsa20Keelung 2 demoSalsa20InputUInt32) [] []
    putStrLn $ if salsa20_4_computed == map fromIntegral salsa20_4_interpreted then "OK" else "FAIL!"

    salsa20_4_compiled <- compile bn128 (salsa20Keelung 2 demoSalsa20InputUInt32)
    let salsa20_4_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") salsa20_4_compiled)
    putStrLn $ if salsa20_4_constraints == 708288 then "OK" else "FAIL!"

    let salsa20_20_computed = salsa20Compute 10 demoSalsa20InputWord32
    salsa20_20_interpreted <- interpret gf181 (salsa20Keelung 10 demoSalsa20InputUInt32) [] []
    putStrLn $ if salsa20_20_computed == map fromIntegral salsa20_20_interpreted then "OK" else "FAIL!"

    salsa20_20_compiled <- compile bn128 (salsa20Keelung 10 demoSalsa20InputUInt32)
    let salsa20_20_constraints = length $ toR1Cs (fromRight (error "error parsing r1cs") salsa20_20_compiled)
    putStrLn $ if salsa20_20_constraints == 853440 then "OK" else "FAIL!"

    -- witness creation
    -- _ <- witness gf181 (coreKeelung demoInputUInt32 2) [] []

    -- just return an empty `Count` so we don't have to return one from a specific test:
    return (Counts 0 0 0 0)
