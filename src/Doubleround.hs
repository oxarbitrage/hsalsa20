{-|
Module      : Doubleround
Description : Doubleround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

Here we define the doubleround function as the composition of rowround and columnround. 
In addition we define `doubleround10`, which is the `doubleround` function applied 10 times,
as specified in the salsa20 spec.
-}
module Doubleround
    (
    doubleroundCompute,
    doubleroundTypeChecker,
    doubleround10Compute,
    --doubleround10TypeChecker,
    ) where

import Data.Word
import Text.Printf

import Rowround
import Columnround
import Utils

-- |The rowround endofunctor.
data ExprF a = Const [(Word32, String)] | Doubleround a

-- |Functor instance.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (Doubleround a) = Doubleround (f a)

-- |Fix and unFix.
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- |The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

-- |The algebra maps for type checking.
algMapsEquation :: ExprF [(Word32, String)] -> [(Word32, String)]
algMapsEquation (Const i) = map passTypeIgnoreValue (listTupleToString i)
algMapsEquation (Doubleround a) = map passTypeIgnoreValue (
    listTupleToString (
        compose
            (Rowround.displayRowRound $ listTupleToNumbers a)
            (Columnround.displayColumnRound $ listTupleToNumbers a)
        )
    )

displayCompose :: (Word32, String) -> (Word32, String) -> (Word32, String)
displayCompose x y = (1, printf "%s . %s" (snd x) (snd y))

compose :: [(Word32, String)] -> [(Word32, String)] -> [(Word32, String)]
compose (x:xs) (y:ys) = zipWith displayCompose (x:xs) (y:ys)
compose _ _ = [(1, "0")]

--
evalTChecker :: Fix ExprF -> [(Word32, String)]
evalTChecker = cata algMapsEquation

-- |The doubleround expression.
doubleroundCompute :: [Word32] -> [Word32]
doubleroundCompute = rowroundCompute . columnroundCompute

--
doubleroundTypeChecker :: [Word32] -> [(Word32, String)]
doubleroundTypeChecker input = evalTChecker $ In $ Doubleround $ In $ Const (addSuffixEmpty input)

-- |The doubleround10 type string.
doubleround10Compute :: [Word32] -> [Word32]
doubleround10Compute = doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute 
    . doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute . doubleroundCompute 
    . doubleroundCompute

-- |The doubleround10 expression.
--doubleround10TypeChecker :: [Word32] -> [String]
--doubleround10TypeChecker = doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker 
--    . doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker . doubleroundTypeChecker 
--    . doubleroundTypeChecker . doubleroundTypeChecker
