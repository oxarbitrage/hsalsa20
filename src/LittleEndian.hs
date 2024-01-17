{-|
Module      : LittleEndian
Description : .
Copyright   : (c) Alfredo Garcia, 2024
License     : MIT
Stability   : experimental
Portability : POSIX



-}
{-# LANGUAGE DataKinds #-}
{-@ LIQUID "--prune-unsorted" @-}

module LittleEndian
    (
        littleendian, extractBytes4, extractBytes8, displayBytes4, displayBytes8,
        LittleEndian.reduce, reduceDisplay, reduceKeelung,
        aument, aumentDisplay, aumentKeelung
    )
where

import Data.Bits
import Data.Word
import Text.Printf

import Keelung hiding (input, eq)

import Operators()

import Utils

-- | Encode a vector as a word using little-endian byte order.
{-@ littleendian :: { i:[Word32] | (len i) == 4 } -> Word32 @-}
littleendian :: [Word32] -> Word32
littleendian bytes = sum [byte `Data.Bits.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

-- |The `littleendian` expression as a string.
{-@ littleendianDisplay :: { i:[String] | (len i) == 4 } -> String @-}
littleendianDisplay :: [String] -> String
littleendianDisplay [b0, b1, b2, b3] = printf "%s + 2^8 * %s + 2^16 * %s + 2^24 * %s" b0 b1 b2 b3
littleendianDisplay _ = ""

-- | Encode a vector as a word using little-endian byte order using Keelung expressions.
{-@ littleendianKeelung :: { i:[_] | (len i) == 4 } -> _ @-}
littleendianKeelung :: [UInt 32] -> UInt 32
littleendianKeelung bytes = sum [byte `Keelung.shiftL` (8 * i) | (i, byte) <- zip [0..] bytes]

{-| Extract 4 bytes from a Word32.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ extractBytes4 :: Word32 -> { o:[Word32] | (len o) == 4 } @-}
extractBytes4 :: Word32 -> [Word32]
extractBytes4 w = [ Data.Bits.shiftR w (8 * 0) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 1) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 2) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 3) Data.Bits..&. 0xff]

{-| Extract 8 bytes from a Word32.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ extractBytes8 :: Word32 -> { o:[Word32] | (len o) == 8 } @-}
extractBytes8 :: Word32 -> [Word32]
extractBytes8 w = [ Data.Bits.shiftR w (8 * 0) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 1) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 2) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 3) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 4) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 5) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 6) Data.Bits..&. 0xff,
        Data.Bits.shiftR w (8 * 7) Data.Bits..&. 0xff]

{-| Display a 4 bytes from a string as a list of strings.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ displayBytes4 :: String -> { o:[String] | (len o) == 4 } @-}
displayBytes4 :: String -> [String]
displayBytes4 w =
    [ printf "%s & 255" w,  printf "%s & 255" w, printf "%s & 255" w, printf "%s & 255" w]

{-| Display a 8 bytes from a string as a list of strings.

This function is implemented as simple haskell (no syntactic sugar) because of liquidhaskell
refinements in list lengths.
-}
{-@ displayBytes8 :: String -> { o:[String] | (len o) == 8 } @-}
displayBytes8 :: String -> [String]
displayBytes8 w =
    [ 
        printf "%s & 255" w,  printf "%s & 255" w, printf "%s & 255" w, printf "%s & 255" w,
        printf "%d >>> %s & 255" ((8 * 4)::Int) w, printf "%d >>> %s & 255" ((8 * 5)::Int) w,
        printf "%d >>> %s & 255" ((8 * 6)::Int) w, printf "%d >>> %s & 255" ((8 * 7)::Int) w]

-- | Extract a specified number of bytes from a Word32 using Keelung expressions.
{-@ extractBytes4Keelung :: _ -> { o:[_] | (len o) == 4 } @-}
extractBytes4Keelung :: UInt 32 -> [UInt 32]
extractBytes4Keelung w = [ Keelung.shiftR w (8 * 0) Keelung..&. 0xff,
        Keelung.shiftR w (8 * 1) Keelung..&. 0xff,
        Keelung.shiftR w (8 * 2) Keelung..&. 0xff,
        Keelung.shiftR w (8 * 3) Keelung..&. 0xff]

-- | Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendian` encoding.
{-@ reduce :: { i:[Word32] | (len i) == 64 } -> { o:[Word32] | (len o) == 16 } @-}
reduce :: [Word32] -> [Word32]
reduce input = Prelude.map littleendian (chunksof4v1 input)

-- | Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianDisplay` encoding.
{-@ reduceDisplay :: { i:[String] | (len i) == 64 } -> { o:[String] | (len o) == 16 } @-}
reduceDisplay :: [String] -> [String]
reduceDisplay input = Prelude.map littleendianDisplay (chunksof4v2 input)

-- |Reduce a matrix of 64 elements to a matrix of 16 elements by using `littleendianKeelung` encoding.
{-@ reduceKeelung :: { i:[_] | (len i) == 64 } -> { o:[_] | (len o) == 16 } @-}
reduceKeelung :: [UInt 32] -> [UInt 32]
reduceKeelung input = Prelude.map littleendianKeelung (chunksof4v3 input)

-- | Aument a matrix of 16 elements to one of 64 elements by using `extractBytes`.
{-@ ignore aument @-}
{-@ aument :: { i:[Word32] | (len i) == 16 } -> { o:[Word32] | (len o) == 64 } @-}
aument :: [Word32] -> [Word32]
aument input = [
    extractBytes4 (input!!0)!!0, extractBytes4 (input!!0)!!1, extractBytes4 (input!!0)!!2, extractBytes4 (input!!0)!!3,
    extractBytes4 (input!!1)!!0, extractBytes4 (input!!1)!!1, extractBytes4 (input!!1)!!2, extractBytes4 (input!!1)!!3,
    extractBytes4 (input!!2)!!0, extractBytes4 (input!!2)!!1, extractBytes4 (input!!2)!!2, extractBytes4 (input!!2)!!3,
    extractBytes4 (input!!3)!!0, extractBytes4 (input!!3)!!1, extractBytes4 (input!!3)!!2, extractBytes4 (input!!3)!!3,
    extractBytes4 (input!!4)!!0, extractBytes4 (input!!4)!!1, extractBytes4 (input!!4)!!2, extractBytes4 (input!!4)!!3,
    extractBytes4 (input!!5)!!0, extractBytes4 (input!!5)!!1, extractBytes4 (input!!5)!!2, extractBytes4 (input!!5)!!3,
    extractBytes4 (input!!6)!!0, extractBytes4 (input!!6)!!1, extractBytes4 (input!!6)!!2, extractBytes4 (input!!6)!!3,
    extractBytes4 (input!!7)!!0, extractBytes4 (input!!7)!!1, extractBytes4 (input!!7)!!2, extractBytes4 (input!!7)!!3,
    extractBytes4 (input!!8)!!0, extractBytes4 (input!!8)!!1, extractBytes4 (input!!8)!!2, extractBytes4 (input!!8)!!3,
    extractBytes4 (input!!9)!!0, extractBytes4 (input!!9)!!1, extractBytes4 (input!!9)!!2, extractBytes4 (input!!9)!!3,
    extractBytes4 (input!!10)!!0, extractBytes4 (input!!10)!!1, extractBytes4 (input!!10)!!2, extractBytes4 (input!!10)!!3,
    extractBytes4 (input!!11)!!0, extractBytes4 (input!!11)!!1, extractBytes4 (input!!11)!!2, extractBytes4 (input!!11)!!3,
    extractBytes4 (input!!12)!!0, extractBytes4 (input!!12)!!1, extractBytes4 (input!!12)!!2, extractBytes4 (input!!12)!!3,
    extractBytes4 (input!!13)!!0, extractBytes4 (input!!13)!!1, extractBytes4 (input!!13)!!2, extractBytes4 (input!!13)!!3,
    extractBytes4 (input!!14)!!0, extractBytes4 (input!!14)!!1, extractBytes4 (input!!14)!!2, extractBytes4 (input!!14)!!3,
    extractBytes4 (input!!15)!!0, extractBytes4 (input!!15)!!1, extractBytes4 (input!!15)!!2, extractBytes4 (input!!15)!!3]

-- | Aument a matrix of 16 elements to one of 64 elements by using `displayBytes`.
{-@ ignore aumentDisplay @-}
{-@ aumentDisplay :: { i:[String] | (len i) == 16 } -> { o:[String] | (len o) == 64 } @-}
aumentDisplay :: [String] -> [String]
aumentDisplay input = [
    displayBytes4 (input!!0)!!0, displayBytes4 (input!!0)!!1, displayBytes4 (input!!0)!!2, displayBytes4 (input!!0)!!3,
    displayBytes4 (input!!1)!!0, displayBytes4 (input!!1)!!1, displayBytes4 (input!!1)!!2, displayBytes4 (input!!1)!!3,
    displayBytes4 (input!!2)!!0, displayBytes4 (input!!2)!!1, displayBytes4 (input!!2)!!2, displayBytes4 (input!!2)!!3,
    displayBytes4 (input!!3)!!0, displayBytes4 (input!!3)!!1, displayBytes4 (input!!3)!!2, displayBytes4 (input!!3)!!3,
    displayBytes4 (input!!4)!!0, displayBytes4 (input!!4)!!1, displayBytes4 (input!!4)!!2, displayBytes4 (input!!4)!!3,
    displayBytes4 (input!!5)!!0, displayBytes4 (input!!5)!!1, displayBytes4 (input!!5)!!2, displayBytes4 (input!!5)!!3,
    displayBytes4 (input!!6)!!0, displayBytes4 (input!!6)!!1, displayBytes4 (input!!6)!!2, displayBytes4 (input!!6)!!3,
    displayBytes4 (input!!7)!!0, displayBytes4 (input!!7)!!1, displayBytes4 (input!!7)!!2, displayBytes4 (input!!7)!!3,
    displayBytes4 (input!!8)!!0, displayBytes4 (input!!8)!!1, displayBytes4 (input!!8)!!2, displayBytes4 (input!!8)!!3,
    displayBytes4 (input!!9)!!0, displayBytes4 (input!!9)!!1, displayBytes4 (input!!9)!!2, displayBytes4 (input!!9)!!3,
    displayBytes4 (input!!10)!!0, displayBytes4 (input!!10)!!1, displayBytes4 (input!!10)!!2, displayBytes4 (input!!10)!!3,
    displayBytes4 (input!!11)!!0, displayBytes4 (input!!11)!!1, displayBytes4 (input!!11)!!2, displayBytes4 (input!!11)!!3,
    displayBytes4 (input!!12)!!0, displayBytes4 (input!!12)!!1, displayBytes4 (input!!12)!!2, displayBytes4 (input!!12)!!3,
    displayBytes4 (input!!13)!!0, displayBytes4 (input!!13)!!1, displayBytes4 (input!!13)!!2, displayBytes4 (input!!13)!!3,
    displayBytes4 (input!!14)!!0, displayBytes4 (input!!14)!!1, displayBytes4 (input!!14)!!2, displayBytes4 (input!!14)!!3,
    displayBytes4 (input!!15)!!0, displayBytes4 (input!!15)!!1, displayBytes4 (input!!15)!!2, displayBytes4 (input!!15)!!3]

-- |Aument a matrix of 16 elements to one of 64 elements by using `extractBytesKeelung`.
{-@ ignore aumentKeelung @-}
{-@ aumentKeelung ::  { i:[_] | (len i) == 16 } -> { o:[_] | (len o) == 64 } @-}
aumentKeelung :: [UInt 32] -> [UInt 32]
aumentKeelung input = [
    extractBytes4Keelung (input!!0)!!0, extractBytes4Keelung (input!!0)!!1, extractBytes4Keelung (input!!0)!!2, extractBytes4Keelung (input!!0)!!3,
    extractBytes4Keelung (input!!1)!!0, extractBytes4Keelung (input!!1)!!1, extractBytes4Keelung (input!!1)!!2, extractBytes4Keelung (input!!1)!!3,
    extractBytes4Keelung (input!!2)!!0, extractBytes4Keelung (input!!2)!!1, extractBytes4Keelung (input!!2)!!2, extractBytes4Keelung (input!!2)!!3,
    extractBytes4Keelung (input!!3)!!0, extractBytes4Keelung (input!!3)!!1, extractBytes4Keelung (input!!3)!!2, extractBytes4Keelung (input!!3)!!3,
    extractBytes4Keelung (input!!4)!!0, extractBytes4Keelung (input!!4)!!1, extractBytes4Keelung (input!!4)!!2, extractBytes4Keelung (input!!4)!!3,
    extractBytes4Keelung (input!!5)!!0, extractBytes4Keelung (input!!5)!!1, extractBytes4Keelung (input!!5)!!2, extractBytes4Keelung (input!!5)!!3,
    extractBytes4Keelung (input!!6)!!0, extractBytes4Keelung (input!!6)!!1, extractBytes4Keelung (input!!6)!!2, extractBytes4Keelung (input!!6)!!3,
    extractBytes4Keelung (input!!7)!!0, extractBytes4Keelung (input!!7)!!1, extractBytes4Keelung (input!!7)!!2, extractBytes4Keelung (input!!7)!!3,
    extractBytes4Keelung (input!!8)!!0, extractBytes4Keelung (input!!8)!!1, extractBytes4Keelung (input!!8)!!2, extractBytes4Keelung (input!!8)!!3,
    extractBytes4Keelung (input!!9)!!0, extractBytes4Keelung (input!!9)!!1, extractBytes4Keelung (input!!9)!!2, extractBytes4Keelung (input!!9)!!3,
    extractBytes4Keelung (input!!10)!!0, extractBytes4Keelung (input!!10)!!1, extractBytes4Keelung (input!!10)!!2, extractBytes4Keelung (input!!10)!!3,
    extractBytes4Keelung (input!!11)!!0, extractBytes4Keelung (input!!11)!!1, extractBytes4Keelung (input!!11)!!2, extractBytes4Keelung (input!!11)!!3,
    extractBytes4Keelung (input!!12)!!0, extractBytes4Keelung (input!!12)!!1, extractBytes4Keelung (input!!12)!!2, extractBytes4Keelung (input!!12)!!3,
    extractBytes4Keelung (input!!13)!!0, extractBytes4Keelung (input!!13)!!1, extractBytes4Keelung (input!!13)!!2, extractBytes4Keelung (input!!13)!!3,
    extractBytes4Keelung (input!!14)!!0, extractBytes4Keelung (input!!14)!!1, extractBytes4Keelung (input!!14)!!2, extractBytes4Keelung (input!!14)!!3,
    extractBytes4Keelung (input!!15)!!0, extractBytes4Keelung (input!!15)!!1, extractBytes4Keelung (input!!15)!!2, extractBytes4Keelung (input!!15)!!3]
