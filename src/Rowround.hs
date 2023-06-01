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

import Types (VectorType, MatrixType, MatrixStringType, VectorStringType)
import Quarterround

import Text.Printf
import Data.Tuple.Select
import Data.Word

-- |The rowround endofunctor.
data ExprF a = Const VectorType | Quarterround a

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
algMapsCompute :: ExprF VectorType -> VectorType
algMapsCompute (Const i) = i
algMapsCompute (Quarterround a) = Quarterround.quarterroundCompute a

-- |The algebra maps.
algMapsDisplay :: ExprF VectorStringType -> VectorStringType
algMapsDisplay (Const i) = (
    printf "%d" (sel1 i),
    printf "%d" (sel2 i),
    printf "%d" (sel3 i),
    printf "%d" (sel4 i))
algMapsDisplay (Quarterround a) = Quarterround.quarterroundDisplay (vectorStringToVector a)

vectorStringToVector :: VectorStringType -> VectorType
vectorStringToVector (a, b, c, d) = (read a :: Word32, read b :: Word32, read c :: Word32, read d :: Word32)

-- |The rowround evaluator.
evalCompute :: Fix ExprF -> VectorType
evalCompute = cata algMapsCompute

-- |The rowround evaluator.
evalDisplay :: Fix ExprF -> (String, String, String, String)
evalDisplay = cata algMapsDisplay

-- |The first quarterround expression.
quarterround1 :: MatrixType -> Fix ExprF
quarterround1 (a, _, _, _) = In $ 
    Quarterround (In $ Const a)

-- |The second quarterround expression.
quarterround2 :: MatrixType -> Fix ExprF
quarterround2 (_, a, _, _) = In $ 
    Quarterround (In $ Const (sort2 a))

-- |Sort a second input for rowround.
sort2 :: VectorType -> VectorType
sort2 (y4, y5, y6, y7) = (y5, y6, y7, y4) 

-- |Inverse of `sort2`, used to order rowround output.
sort2_inv :: VectorType -> VectorType
sort2_inv (z5, z6, z7, z4) = (z4, z5, z6, z7) 

-- |Inverse of `sort2`, used to order rowround output when objects are strings.
sort2_inv' :: VectorStringType -> VectorStringType
sort2_inv' (z5, z6, z7, z4) = (z4, z5, z6, z7)

-- |The third quarterround expression.
quarterround3 :: MatrixType -> Fix ExprF
quarterround3 (_, _, a, _) = In $ 
    Quarterround (In $ Const (sort3 a))

-- |Sort a third input for rowround.
sort3 :: VectorType -> VectorType
sort3 (y8, y9, y10, y11) = (y10, y11, y8, y9) 

-- |Inverse of `sort3`, used to order rowround output.
sort3_inv :: VectorType -> VectorType
sort3_inv (z10, z11, z8, z9) = (z8, z9, z10, z11) 

-- |Inverse of `sort3`, used to order rowround output when objects are strings.
sort3_inv' :: VectorStringType -> VectorStringType
sort3_inv' (z10, z11, z8, z9) = (z8, z9, z10, z11)

-- |The fourth quarterround expression.
quarterround4 :: MatrixType -> Fix ExprF
quarterround4 (_, _, _, a) = In $ 
    Quarterround (In $ Const (sort4 a))

-- |Sort a fourth input for rowround.
sort4 :: VectorType -> VectorType
sort4 (y12, y13, y14, y15) = (y15, y12, y13, y14) 

-- |Inverse of `sort4`, used to order rowround output.
sort4_inv :: VectorType -> VectorType
sort4_inv (z15, z12, z13, z14) = (z12, z13, z14, z15)

-- |Inverse of `sort4`, used to order rowround output when objects are strings.
sort4_inv' :: VectorStringType -> VectorStringType
sort4_inv' (z15, z12, z13, z14) = (z12, z13, z14, z15)

-- |The rowround expression computed.
rowroundCompute :: MatrixType -> MatrixType
rowroundCompute input = (evalCompute $ quarterround1 input, sort2_inv(evalCompute $ quarterround2 input),
    sort3_inv(evalCompute $ quarterround3 input), sort4_inv(evalCompute $ quarterround4 input))

-- |The rowround expression as a string.
rowroundDisplay :: MatrixType -> MatrixStringType
rowroundDisplay input = (evalDisplay $ quarterround1 input, sort2_inv'(evalDisplay $ quarterround2 input),
    sort3_inv'(evalDisplay $ quarterround3 input), sort4_inv'(evalDisplay $ quarterround4 input))
