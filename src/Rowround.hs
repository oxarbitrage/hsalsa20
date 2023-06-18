{-|
Module      : Rowround
Description : Rowround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We implement the rowround equations as an F-Algebra where `Quarterround` is its single operation.
This allow us to form rowround expressions and evaluate them.

We evaluate 3 different things:

- The numeric value.
- The type string.
- The equations.
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


fff1 :: (Word32, String) -> Word32
fff1 = fst

tupleToList1 :: [(Word32, String)] -> [Word32]
tupleToList1 = map fff1

tupleToList2 :: [(Word32, String)] -> [String]
tupleToList2 = map snd

tupleToList3 :: [String] -> [(Word32, String)]
tupleToList3 =  map f1''

f1'' :: String  -> (Word32, String)
f1'' i = (1, i) 

f1 :: a -> (a, String)
f1 a = (a, "_r1") 
f2 :: a -> (a, String)
f2 a = (a, "_r2") 
f3 :: a -> (a, String)
f3 a = (a, "_r3") 
f4 :: a -> (a, String)
f4 a = (a, "_r4") 


tupleToList1' :: [Word32] -> [(Word32, String)]
tupleToList1' =  map f1

tupleToList2' :: [Word32] -> [(Word32, String)]
tupleToList2' =  map f2

tupleToList3' :: [Word32] -> [(Word32, String)]
tupleToList3' =  map f3

tupleToList4' :: [Word32] -> [(Word32, String)]
tupleToList4' = map f4

-- |The algebra maps for computation.
algMapsCompute :: ExprF [(Word32, String)] -> [(Word32, String)]
algMapsCompute (Const i) = i
algMapsCompute (Quarterround a) = tupleToList4' (Quarterround.quarterroundCompute (tupleToList1 a))

passTypeIgnoreValue :: String -> (Word32, String)
passTypeIgnoreValue s = (1, printf "%s" s)

-- |The algebra maps for computation.
algMapsEquation :: ExprF [(Word32, String)] -> [(Word32, String)]
algMapsEquation (Const i) = map passTypeIgnoreValue (tupleToList2 i)
algMapsEquation (Quarterround a) = tupleToList3 (Quarterround.quarterroundTypeChecker a)

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
quarterround1 a = In $ Quarterround (In $ Const (head (chunksOf 4 (tupleToList1' a))))

-- |The second quarterround expression.
quarterround2 :: [Word32] -> Fix ExprF
quarterround2 a = In $ Quarterround (In $ Const (sort2 (chunksOf 4 (tupleToList2' a)!!1)))

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
quarterround3 a = In $ Quarterround (In $ Const (sort3 (chunksOf 4 (tupleToList3' a)!!2)))

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
quarterround4 a = In $ Quarterround (In $ Const (sort4 (chunksOf 4 (tupleToList4' a)!!3)))

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
rowroundCompute input = concat [tupleToList1 (evalCompute $ quarterround1 input), 
    sort2_inv( tupleToList1 (evalCompute $ quarterround2 input)),
    sort3_inv( tupleToList1 (evalCompute $ quarterround3 input)), 
    sort4_inv( tupleToList1 ( evalCompute $ quarterround4 input))]

-- |The rowround expression as a string.
rowroundTypeChecker :: [Word32] -> [(Word32, String)]
rowroundTypeChecker input = concat [evalTChecker $ quarterround1 input, sort2_inv(evalTChecker $ quarterround2 input),
    sort3_inv(evalTChecker $ quarterround3 input), sort4_inv(evalTChecker $ quarterround4 input)]
