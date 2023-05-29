{-|
Module      : Quarterround
Description : Quarterround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We treat the quarterround equations defined in the spec as an F-Algebra by defining `Mod`, `Rotl` and `Xor2` as operations.
This allow us to form quarterround expressions and evaluate them.
-}
module Quarterround
    (
    quarterroundCompute,
    quarterroundDisplay
    ) where

import Data.Bits
import Data.Word
import Text.Printf

import Types (VectorType)

-- |The quarterround endofunctor
data ExprF a = Const Word32
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- |Quarterround functor instance. Needed if no automatic derive is done.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (left `Mod` right) = f left `Mod` f right
    fmap f (left `Xor2` right) = f left `Xor2` f right
    fmap f (Rotl7 a) = Rotl7 (f a)
    fmap f (Rotl9 a) = Rotl9 (f a)
    fmap f (Rotl13 a) = Rotl13 (f a)
    fmap f (Rotl18 a) = Rotl18 (f a)

-- |Fix and unFix.
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- |The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

-- |The F-algebra maps for a `Word32` evaluator.
algMapsCompute :: ExprF Word32 -> Word32
algMapsCompute (Const i) = i
algMapsCompute (a `Mod` b) = a + b
algMapsCompute (Rotl7 a) = rotate a 7
algMapsCompute (Rotl9 a) = rotate a 9
algMapsCompute (Rotl13 a) = rotate a 13
algMapsCompute (Rotl18 a) = rotate a 18
algMapsCompute (a `Xor2` b) = xor a b

-- |The F-algebra maps for a `String` evaluator. For expression displaying purposes.
algMapsDisplay :: ExprF String -> String
algMapsDisplay (Const i) = printf "%d" i
algMapsDisplay (a `Mod` b) = printf "(%s + %s)" a b
algMapsDisplay (Rotl7 a) = printf "(%s <<< 7)" a
algMapsDisplay (Rotl9 a) = printf "(%s <<< 9)" a
algMapsDisplay (Rotl13 a) = printf "(%s <<< 13)" a
algMapsDisplay (Rotl18 a) = printf "(%s <<< 18)" a
algMapsDisplay (a `Xor2` b) = printf "%s ⊕ %s" a b

-- |The quarterround evaluator that will do the computation.
evalCompute :: Fix ExprF -> Word32
evalCompute = cata algMapsCompute

-- |The quarterround evaluator that will display expressions.
evalDisplay :: Fix ExprF -> String
evalDisplay = cata algMapsDisplay

-- |The right hand side of the `z1` expression as an expression. `((y0 + y3) <<< 7)`
rhs1 :: VectorType -> Fix ExprF
rhs1 (y0, _, _, y3) = In $ 
    Rotl7 (In $ In (Const y0) `Mod` In (Const y3))

-- |The `z1` expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
z1 :: VectorType -> Fix ExprF
z1 (y0, y1, y2, y3) = In $ 
  In (Const y1) `Xor2` rhs1 (y0, y1, y2, y3)

-- |The right hand side of the `z2` expression as an expression. `((z1 + y0) <<< 9)`
rhs2 :: VectorType -> Fix ExprF
rhs2 (y0, y1, y2, y3) = In $ 
    Rotl9 (In $ z1 (y0, y1, y2, y3) `Mod` In (Const y0))

-- |The `z2` expression. `y2 ⊕ ((z1 + y0) <<< 9)`
z2 :: VectorType -> Fix ExprF
z2 (y0, y1, y2, y3) = In $ 
    In (Const y2) `Xor2` rhs2 (y0, y1, y2, y3)

-- |The right hand side of the `z3` expression as an expression. `(z2 + z1) <<< 13)`
rhs3 :: VectorType -> Fix ExprF
rhs3 (y0, y1, y2, y3) = In $ 
    Rotl13 (In $ z2 (y0, y1, y2, y3)  `Mod` z1 (y0, y1, y2, y3))

-- |The `z3` expression. `y3 ⊕ ((z2 + z1) <<< 13)`
z3 :: VectorType -> Fix ExprF
z3 (y0, y1, y2, y3) = In $ 
    In (Const y3) `Xor2` rhs3 (y0, y1, y2, y3)

-- |The right hand side of the `z0` expression as an expression. `((z3 + z2) <<< 18)`
rhs0 :: VectorType -> Fix ExprF
rhs0 (y0, y1, y2, y3) = In $ 
    Rotl18 (In $ z3 (y0, y1, y2, y3) `Mod` z2 (y0, y1, y2, y3))

-- |The `z0` expression. `y0 ⊕ ((z3 + z2) <<< 18)`
z0 :: VectorType -> Fix ExprF
z0 (y0, y1, y2, y3) = In $ 
    In (Const y0) `Xor2` rhs0 (y0, y1, y2, y3)

-- |The quarterround expression computed. `quarterround(y) = (z0, z1, z2, z3)`
quarterroundCompute :: VectorType -> VectorType
quarterroundCompute (y0, y1, y2, y3) = (evalCompute $ z0 (y0, y1, y2, y3), evalCompute $ z1 (y0, y1, y2, y3),
    evalCompute $ z2 (y0, y1, y2, y3), evalCompute $ z3 (y0, y1, y2, y3))

-- |The quarterround expression as a `String` for displaying expression purposes.
quarterroundDisplay :: VectorType -> (String, String, String, String)
quarterroundDisplay (y0, y1, y2, y3) = (evalDisplay $ z0 (y0, y1, y2, y3), evalDisplay $ z1 (y0, y1, y2, y3),
    evalDisplay $ z2 (y0, y1, y2, y3), evalDisplay $ z3 (y0, y1, y2, y3))
