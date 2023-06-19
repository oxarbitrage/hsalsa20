{-|
Module      : Rowround
Description : Rowround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We implement the rowround equations as an F-Algebra where `Quarterround` is its single operation.
This allow us to form rowround expressions and evaluate them.

We evaluate2 different things:

- The numeric value.
- The type string.
-}
module Rowround
    (
    rowroundCompute,
    rowroundTypeChecker,
    stringList2numbers,
    ) where

import Text.Printf
import Data.Word
import Data.List.Split (chunksOf)

import Quarterround

-- |The rowround endofunctor.
data ExprF a = Const [(Word32, String)] | Quarterround a

-- |Functor instance.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (Quarterround a) = Quarterround (f a)

-- |Fix and unFix.
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- |The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

listTupleToNumber :: [(Word32, String)] -> [Word32]
listTupleToNumber = map fst

listTupleToString :: [(Word32, String)] -> [String]
listTupleToString = map snd

addSuffixRow1 :: [Word32] -> [(Word32, String)]
addSuffixRow1 =  map (\x -> (x, "_r1"))

addSuffixRow2 :: [Word32] -> [(Word32, String)]
addSuffixRow2 = map (\x -> (x, "_r2"))

addSuffixRow3 :: [Word32] -> [(Word32, String)]
addSuffixRow3 =  map (\x -> (x, "_r3"))

addSuffixRow4 :: [Word32] -> [(Word32, String)]
addSuffixRow4 = map (\x -> (x, "_r4"))

addSuffixEmpty :: [Word32] -> [(Word32, String)]
addSuffixEmpty = map (\x -> (x, ""))

-- |The algebra maps for computation.
algMapsCompute :: ExprF [(Word32, String)] -> [(Word32, String)]
algMapsCompute (Const i) = i
algMapsCompute (Quarterround a) = addSuffixEmpty $ Quarterround.quarterroundCompute (listTupleToNumber a)

passTypeIgnoreValue :: String -> (Word32, String)
passTypeIgnoreValue s = (1, printf "%s" s)

-- |The algebra maps for computation.
algMapsEquation :: ExprF [(Word32, String)] -> [(Word32, String)]
algMapsEquation (Const i) = map passTypeIgnoreValue (listTupleToString i)
algMapsEquation (Quarterround a) = map passTypeIgnoreValue (Quarterround.quarterroundTypeChecker a)

-- | A function to convert a list of string to a list of numbers.
stringList2numbers :: [String] -> [Word32]
stringList2numbers [a, b, c, d] = [read a :: Word32, read b :: Word32, read c :: Word32, read d :: Word32]
stringList2numbers _ = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

-- |The rowround evaluator.
evalCompute :: Fix ExprF -> [(Word32, String)]
evalCompute = cata algMapsCompute

-- |The rowround evaluator as a string.
evalTChecker :: Fix ExprF -> [(Word32, String)]
evalTChecker = cata algMapsEquation

-- |The first quarterround expression.
quarterround1 :: [Word32] -> Fix ExprF
quarterround1 a = In $ Quarterround $ In $ Const $ head $ chunksOf 4 (addSuffixRow1 a)

-- |The second quarterround expression.
quarterround2 :: [Word32] -> Fix ExprF
quarterround2 a = In $ Quarterround $ In $ Const $ sort2 $ chunksOf 4 (addSuffixRow2 a)!!1

-- |Sort a second input for rowround.
sort2 :: [a] -> [a]
sort2 [y4, y5, y6, y7] = [y5, y6, y7, y4] 
sort2 _ = []

-- |Inverse of `sort2`, used to order rowround output.
sort2_inv :: [a] -> [a]
sort2_inv [z5, z6, z7, z4] = [z4, z5, z6, z7] 
sort2_inv _ = []

-- |The third quarterround expression.
quarterround3 :: [Word32] -> Fix ExprF
quarterround3 a = In $ Quarterround $ In $ Const $ sort3 $ chunksOf 4 (addSuffixRow3 a)!!2

-- |Sort a third input for rowround.
sort3 :: [a] -> [a]
sort3 [y8, y9, y10, y11] = [y10, y11, y8, y9] 
sort3 _ = []

-- |Inverse of `sort3`, used to order rowround output.
sort3_inv :: [a] -> [a]
sort3_inv [z10, z11, z8, z9] = [z8, z9, z10, z11] 
sort3_inv _ = []

-- |The fourth quarterround expression.
quarterround4 :: [Word32] -> Fix ExprF
quarterround4 a = In $ Quarterround $ In $ Const $ sort4 $ chunksOf 4 (addSuffixRow4 a)!!3

-- |Sort a fourth input for rowround.
sort4 :: [a] -> [a]
sort4 [y12, y13, y14, y15] = [y15, y12, y13, y14] 
sort4 _ = []

-- |Inverse of `sort4`, used to order rowround output.
sort4_inv :: [a] -> [a]
sort4_inv [z15, z12, z13, z14] = [z12, z13, z14, z15]
sort4_inv _ = []

-- |The rowround expression computed.
rowroundCompute :: [Word32] -> [Word32]
rowroundCompute input = concat [
    listTupleToNumber $ evalCompute $ quarterround1 input,
    sort2_inv $ listTupleToNumber $ evalCompute $ quarterround2 input,
    sort3_inv $ listTupleToNumber $ evalCompute $ quarterround3 input, 
    sort4_inv $ listTupleToNumber $ evalCompute $ quarterround4 input]

-- |The rowround expression as a string.
rowroundTypeChecker :: [Word32] -> [(Word32, String)]
rowroundTypeChecker input = concat [
    evalTChecker $ quarterround1 input,
    sort2_inv $ evalTChecker $ quarterround2 input,
    sort3_inv $ evalTChecker $ quarterround3 input,
    sort4_inv $ evalTChecker $ quarterround4 input]
