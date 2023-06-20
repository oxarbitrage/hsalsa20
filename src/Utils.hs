{-|
Module      : Utils
Description : Utilities
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

General utility functions used to create the salsa20 cipher.
-}
module Utils
    (
        littleendian,
        littleendianInv,
        reduce,
        aument,
        modMatrix,
        littleendianInv4Crypt,
        listNumbersToPair,
        listTupleToNumbers,
        listTupleToString,
        addSuffixRow1,
        addSuffixRow2,
        addSuffixRow3,
        addSuffixRow4,
        addSuffixEmpty,
        textToTuple,
        replaceInitialRowround,
        replaceInitialColumnround,
    ) where

import Data.Bits
import Data.Word
import Data.List.Split
import qualified Data.Text as T

-- |Raise 2 to the power of `p`.
power :: Word32 -> Word32
power p = 2 ^ p 

-- |Encode a vector as a word using protocol specified littleendian. 
littleendian :: [Word32] -> Word32
littleendian [b0, b1, b2, b3] = b0 + power 8 * b1 + power 16 * b2 + power 24 * b3
littleendian _ = 0

-- |The inverse of `littleendian`. Implemented as specified in https://crypto.stackexchange.com/a/22314.
littleendianInv :: Word32 -> [Word32]
littleendianInv w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff]

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
reduce :: [Word32] -> [Word32]
reduce input = map littleendian $ chunksOf 4 input

-- |Aument a matrix of 16 elements to one of 64 elements by using `littleendianInv`.
aument :: [Word32] -> [Word32]
aument = concatMap littleendianInv

-- |Given two matrices, do modulo addition on each of the elements.
modMatrix :: [Word32] -> [Word32] -> [Word32]
modMatrix = zipWith (+)

-- |The littleendian function used in encryption/decryption where a number is obtained given a list of 8 bytes.
littleendianInv4Crypt ::  Word32 -> [Word32]
littleendianInv4Crypt w = [w .&. 0xff, shiftR w 8 .&. 0xff, shiftR w 16 .&. 0xff, shiftR w 24 .&. 0xff,
    shiftR w 32 .&. 0xff, shiftR 40 8 .&. 0xff, shiftR w 48 .&. 0xff, shiftR w 56 .&. 0xff]


--
listNumbersToPair :: [Word32] -> [(Word32, String)]
listNumbersToPair = map (\x -> (x, ""))

--
listTupleToNumbers :: [(Word32, String)] -> [Word32]
listTupleToNumbers = map fst

--
listTupleToString :: [(Word32, String)] -> [String]
listTupleToString = map snd

--
addSuffixRow1 :: [Word32] -> [(Word32, String)]
addSuffixRow1 =  map (\x -> (x, "_r1"))

--
addSuffixRow2 :: [Word32] -> [(Word32, String)]
addSuffixRow2 = map (\x -> (x, "_r2"))

--
addSuffixRow3 :: [Word32] -> [(Word32, String)]
addSuffixRow3 =  map (\x -> (x, "_r3"))

--
addSuffixRow4 :: [Word32] -> [(Word32, String)]
addSuffixRow4 = map (\x -> (x, "_r4"))

--
addSuffixEmpty :: [Word32] -> [(Word32, String)]
addSuffixEmpty = map (\x -> (x, ""))

--
textToTuple :: [T.Text] -> [(Word32, String)]
textToTuple = map (\x -> (1, (T.unpack x)))

--
replaceInitialRowround :: [T.Text] -> [(Word32, String)]
replaceInitialRowround orig = do
    let l0 = map (T.replace (T.pack "y0_r1") (T.pack "y0")) orig
    let l1 = map (T.replace (T.pack "y1_r1") (T.pack "y1")) l0
    let l2 = map (T.replace (T.pack "y2_r1") (T.pack "y2")) l1
    let l3 = map (T.replace (T.pack "y3_r1") (T.pack "y3")) l2

    let l4 = map (T.replace (T.pack "y0_r2") (T.pack "y4")) l3
    let l5 = map (T.replace (T.pack "y1_r2") (T.pack "y5")) l4
    let l6 = map (T.replace (T.pack "y2_r2") (T.pack "y6")) l5
    let l7 = map (T.replace (T.pack "y3_r2") (T.pack "y7")) l6

    let l8 = map (T.replace (T.pack "y0_r3") (T.pack "y8")) l7
    let l9 = map (T.replace (T.pack "y1_r3") (T.pack "y9")) l8
    let l10 = map (T.replace (T.pack "y2_r3") (T.pack "y10")) l9
    let l11 = map (T.replace (T.pack "y3_r3") (T.pack "y11")) l10

    let l12 = map (T.replace (T.pack "y0_r4") (T.pack "y11")) l11
    let l13 = map (T.replace (T.pack "y1_r4") (T.pack "y12")) l12
    let l14 = map (T.replace (T.pack "y2_r4") (T.pack "y13")) l13
    let l15 = map (T.replace (T.pack "y3_r4") (T.pack "y14")) l14

    textToTuple l15


replaceInitialColumnround :: [T.Text] -> [(Word32, String)]
replaceInitialColumnround orig = do
    let l0 = map (T.replace (T.pack "y0_r1") (T.pack "x0")) orig
    let l1 = map (T.replace (T.pack "y1_r1") (T.pack "x1")) l0
    let l2 = map (T.replace (T.pack "y2_r1") (T.pack "x2")) l1
    let l3 = map (T.replace (T.pack "y3_r1") (T.pack "x3")) l2

    let l4 = map (T.replace (T.pack "y0_r2") (T.pack "x4")) l3
    let l5 = map (T.replace (T.pack "y1_r2") (T.pack "x5")) l4
    let l6 = map (T.replace (T.pack "y2_r2") (T.pack "x6")) l5
    let l7 = map (T.replace (T.pack "y3_r2") (T.pack "x7")) l6

    let l8 = map (T.replace (T.pack "y0_r3") (T.pack "x8")) l7
    let l9 = map (T.replace (T.pack "y1_r3") (T.pack "x9")) l8
    let l10 = map (T.replace (T.pack "y2_r3") (T.pack "x10")) l9
    let l11 = map (T.replace (T.pack "y3_r3") (T.pack "x11")) l10

    let l12 = map (T.replace (T.pack "y0_r4") (T.pack "x12")) l11
    let l13 = map (T.replace (T.pack "y1_r4") (T.pack "x13")) l12
    let l14 = map (T.replace (T.pack "y2_r4") (T.pack "x14")) l13
    let l15 = map (T.replace (T.pack "y3_r4") (T.pack "x15")) l14

    textToTuple l15
