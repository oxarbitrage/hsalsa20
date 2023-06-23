{-|
Module      : Quarterround
Description : Quarterround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We implement the quarterround equations defined in the spec as an F-Algebra where `Mod`, `Rotl` and `Xor2` are operations.
This allow us to form quarterround expressions and evaluate them.

We evaluate 2 things:

- The numeric values.
- The type string.

-}
module Quarterround
    (
    quarterroundCompute,
    quarterroundDisplay,
    quarterroundEquation,
    ) where

import Data.Bits
import Data.Word
import Text.Printf

import Utils

-- |The quarterround endofunctor to compute a value and a string type.
data ExprF a = Const String
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- |Quarterround functor instance for computing a value and displaying a type string.
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
algMapsCompute (Const i) = read i :: Word32
algMapsCompute (a `Mod` b) = a + b
algMapsCompute (Rotl7 a) = rotate a 7
algMapsCompute (Rotl9 a) = rotate a 9
algMapsCompute (Rotl13 a) = rotate a 13
algMapsCompute (Rotl18 a) = rotate a 18
algMapsCompute (a `Xor2` b) = xor a b

-- |The F-algebra maps for a `String` evaluator.
algMapsDisplay :: ExprF String -> String
algMapsDisplay (Const i) = printf "%s" i
algMapsDisplay (a `Mod` b) = printf "(%s + %s)" a b
algMapsDisplay (Rotl7 a) = printf "(%s <<< 7)" a
algMapsDisplay (Rotl9 a) = printf "(%s <<< 9)" a
algMapsDisplay (Rotl13 a) = printf "(%s <<< 13)" a
algMapsDisplay (Rotl18 a) = printf "(%s <<< 18)" a
algMapsDisplay (a `Xor2` b) = printf "%s ⊕ %s" a b

-- |The quarterround evaluator that will do the computation.
evalCompute :: Fix ExprF -> Word32
evalCompute = cata algMapsCompute

-- |The quarterround evaluator that will display equations.
evalDisplay :: Fix ExprF -> String
evalDisplay = cata algMapsDisplay

-- |The right hand side of the `z1` expression as an expression. `((y0 + y3) <<< 7)`
rhs1 :: [String] -> Fix ExprF
rhs1 [y0, _, _, y3] = In $ Rotl7 (In $ In (Const y0) `Mod` In (Const y3))
rhs1 _ = error "input to `rhs1` must be a list of 4 `Word32` numbers"

-- |The `z1` expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
z1 :: [String] -> Fix ExprF
z1 [y0, y1, y2, y3] = In $ In (Const y1) `Xor2` rhs1 [y0, y1, y2, y3]
z1 _ = error "input to `z1` must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z2` expression as an expression. `((z1 + y0) <<< 9)`
rhs2 :: [String] -> Fix ExprF
rhs2 [y0, y1, y2, y3] = In $ Rotl9 (In $ z1 [y0, y1, y2, y3] `Mod` In (Const y0))
rhs2 _ = error "input to `rhs2` must be a list of 4 `Word32` numbers"

-- |The `z2` expression. `y2 ⊕ ((z1 + y0) <<< 9)`
z2 :: [String] -> Fix ExprF
z2 [y0, y1, y2, y3] = In $ In (Const y2) `Xor2` rhs2 [y0, y1, y2, y3]
z2 _ = error "input to `z2` must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z3` expression as an expression. `(z2 + z1) <<< 13)`
rhs3 :: [String] -> Fix ExprF
rhs3 [y0, y1, y2, y3] = In $ Rotl13 (In $ z2 [y0, y1, y2, y3]  `Mod` z1 [y0, y1, y2, y3])
rhs3 _ = error "input to `rhs3` must be a list of 4 `Word32` numbers"

-- |The `z3` expression. `y3 ⊕ ((z2 + z1) <<< 13)`
z3 :: [String] -> Fix ExprF
z3 [y0, y1, y2, y3] = In $ In (Const y3) `Xor2` rhs3 [y0, y1, y2, y3]
z3 _ = error "input to `z3` must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z0` expression as an expression. `((z3 + z2) <<< 18)`
rhs0 :: [String] -> Fix ExprF
rhs0 [y0, y1, y2, y3] = In $ Rotl18 (In $ z3 [y0, y1, y2, y3] `Mod` z2 [y0, y1, y2, y3])
rhs0 _ = error "input to `rhs0`  must be a list of 4 `Word32` numbers"

-- |The `z0` expression. `y0 ⊕ ((z3 + z2) <<< 18)`
z0 :: [String] -> Fix ExprF
z0 [y0, y1, y2, y3] = In $ In (Const y0) `Xor2` rhs0 [y0, y1, y2, y3]
z0 _ = error "input to `z0` must be a list of 4 `Word32` numbers"

-- |The quarterround expression computed.
quarterroundCompute :: [Word32] -> [Word32]
quarterroundCompute input = do
    if length input == 4 then
        [
        evalCompute $ z0 $ numberListToStringList input,
        evalCompute $ z1 $ numberListToStringList input,
        evalCompute $ z2 $ numberListToStringList input,
        evalCompute $ z3 $ numberListToStringList input]
    else
        error "input to `quarterroundCompute` must be a list of 4 `Word32` numbers"

-- |The quarterround expression as a string.
quarterroundDisplay :: [String] -> [String]
quarterroundDisplay input = do
    if length input == 4 then
        [
        evalDisplay $ z0 input,
        evalDisplay $ z1 input,
        evalDisplay $ z2 input,
        evalDisplay $ z3 input]
    else
        error "input to `quarterroundDisplay` must be a list of 4 `String` strings"

-- |The quarterround expression as an equation.
quarterroundEquation :: [String] -> [String]
quarterroundEquation input = do
    if length input == 4 then do
            let display = quarterroundDisplay input
            let index0 :: Int = 0
            let displayIndex = zip [index0..] display
            let equation = map (uncurry (printf "z%d = %s")) displayIndex
            equation
    else
        error "input to `quarterroundEquation` must be a list of 4 `String` strings"
