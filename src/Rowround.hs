{-|
Module      : Rowround
Description : Rowround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We treat the rowround algebra as an F-Algebra by defining `Quarterround` as its single operation.
This allow us to form rowround expressions.
-}
module Rowround
    (
    rowroundCompute,
    rowroundDisplay,
    ) where

import Text.Printf
import Data.Word

import Quarterround

-- |The rowround endofunctor.
data ExprF a = Const [Word32] | Quarterround a

-- |Functor instance. Needed if no automatic derive is done.
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

-- |The algebra maps.
algMapsCompute :: ExprF [Word32] -> [Word32]
algMapsCompute (Const i) = i
algMapsCompute (Quarterround a) = Quarterround.quarterroundCompute a

-- |The algebra maps.
algMapsDisplay :: ExprF [String] -> [String]
algMapsDisplay (Const i) = [
    printf "%d" (i!!0),
    printf "%d" (i!!1),
    printf "%d" (i!!2),
    printf "%d" (i!!3)]
algMapsDisplay (Quarterround a) = Quarterround.quarterroundDisplay (vectorStringToVector a)

vectorStringToVector :: [String] -> [Word32]
vectorStringToVector [a, b, c, d] = [read a :: Word32, read b :: Word32, read c :: Word32, read d :: Word32]
vectorStringToVector _ = []

-- |The rowround evaluator.
evalCompute :: Fix ExprF -> [Word32]
evalCompute = cata algMapsCompute

-- |The rowround evaluator as a string.
evalDisplay :: Fix ExprF -> [String]
evalDisplay = cata algMapsDisplay

-- |The first quarterround expression.
quarterround1 :: [Word32] -> Fix ExprF
quarterround1 a = In $ Quarterround (In $ Const [a!!0, a!!1, a!!2, a!!3])

-- |The second quarterround expression.
quarterround2 :: [Word32] -> Fix ExprF
quarterround2 a = In $ Quarterround (In $ Const (sort2 [a!!4, a!!5, a!!6, a!!7]))

-- |Sort a second input for rowround.
sort2 :: [Word32] -> [Word32]
sort2 [y4, y5, y6, y7] = [y5, y6, y7, y4] 
sort2 _ = []

-- |Inverse of `sort2`, used to order rowround output.
sort2_inv :: [Word32] -> [Word32]
sort2_inv [z5, z6, z7, z4] = [z4, z5, z6, z7] 
sort2_inv _ = []

-- |Inverse of `sort2`, used to order rowround output when objects are strings.
sort2_inv' :: [String] -> [String]
sort2_inv' [z5, z6, z7, z4] = [z4, z5, z6, z7]
sort2_inv' _ = []

-- |The third quarterround expression.
quarterround3 :: [Word32] -> Fix ExprF
quarterround3 a = In $ Quarterround (In $ Const (sort3 [a!!8, a!!9, a!!10, a!!11]))

-- |Sort a third input for rowround.
sort3 :: [Word32] -> [Word32]
sort3 [y8, y9, y10, y11] = [y10, y11, y8, y9] 
sort3 _ = []

-- |Inverse of `sort3`, used to order rowround output.
sort3_inv :: [Word32] -> [Word32]
sort3_inv [z10, z11, z8, z9] = [z8, z9, z10, z11] 
sort3_inv _ = []

-- |Inverse of `sort3`, used to order rowround output when objects are strings.
sort3_inv' :: [String] -> [String]
sort3_inv' [z10, z11, z8, z9] = [z8, z9, z10, z11]
sort3_inv' _ = []

-- |The fourth quarterround expression.
quarterround4 :: [Word32] -> Fix ExprF
quarterround4 a = In $ Quarterround (In $ Const (sort4 [a!!12, a!!13, a!!14, a!!15]))

-- |Sort a fourth input for rowround.
sort4 :: [Word32] -> [Word32]
sort4 [y12, y13, y14, y15] = [y15, y12, y13, y14] 
sort4 _ = []

-- |Inverse of `sort4`, used to order rowround output.
sort4_inv :: [Word32] -> [Word32]
sort4_inv [z15, z12, z13, z14] = [z12, z13, z14, z15]
sort4_inv _ = []

-- |Inverse of `sort4`, used to order rowround output when objects are strings.
sort4_inv' :: [String] -> [String]
sort4_inv' [z15, z12, z13, z14] = [z12, z13, z14, z15]
sort4_inv' _ = []

-- |The rowround expression computed.
rowroundCompute :: [Word32] -> [Word32]
rowroundCompute input = concat [evalCompute $ quarterround1 input, sort2_inv(evalCompute $ quarterround2 input),
    sort3_inv(evalCompute $ quarterround3 input), sort4_inv(evalCompute $ quarterround4 input)]

-- |The rowround expression as a string.
rowroundDisplay :: [Word32] -> [String]
rowroundDisplay input = concat [evalDisplay $ quarterround1 input, sort2_inv'(evalDisplay $ quarterround2 input),
    sort3_inv'(evalDisplay $ quarterround3 input), sort4_inv'(evalDisplay $ quarterround4 input)]
