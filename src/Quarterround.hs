{-|
Module      : Quarterround
Description : Quarterround related code
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

We treat the quarterround equations defined in the spec as an F-Algebra by defining `Mod`, `Rotl` and `Xor2` as operations.
This allow us to form quarterround expressions and evaluate them.

We evaluate 3 different things:

- The numeric value.
- The type string.
- The equations.

-}
module Quarterround
    (
    quarterroundCompute,
    quarterroundDisplay,
    quarterroundTypeChecker,
    ) where

import Data.Bits
import Data.Typeable
import Data.Word
import Text.Printf

-- |The quarterround endofunctor to compute a value and a string type.
data ExprF a = Const Word32
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- |The quarterround endofunctor to build an equation.
data ExprFT a = ConstT String
        | ModT a a
        | Rotl7T a
        | Rotl9T a
        | Rotl13T a
        | Rotl18T a
        | Xor2T a a

-- |Quarterround functor instance for computing a value and displaying a type string.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (left `Mod` right) = f left `Mod` f right
    fmap f (left `Xor2` right) = f left `Xor2` f right
    fmap f (Rotl7 a) = Rotl7 (f a)
    fmap f (Rotl9 a) = Rotl9 (f a)
    fmap f (Rotl13 a) = Rotl13 (f a)
    fmap f (Rotl18 a) = Rotl18 (f a)

-- |Quarterround functor instance for displaying equation where input are strings.
instance Functor ExprFT where
    fmap _ (ConstT i) = ConstT i
    fmap f (left `ModT` right) = f left `ModT` f right
    fmap f (left `Xor2T` right) = f left `Xor2T` f right
    fmap f (Rotl7T a) = Rotl7T (f a)
    fmap f (Rotl9T a) = Rotl9T (f a)
    fmap f (Rotl13T a) = Rotl13T (f a)
    fmap f (Rotl18T a) = Rotl18T (f a)

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

-- |The F-algebra maps for a `String` evaluator.
algMapsDisplay :: ExprF String -> String
algMapsDisplay (Const i) = printf "(%d : %s)" i (show (typeOf i))
algMapsDisplay (a `Mod` b) = printf "(%s + %s)" a b
algMapsDisplay (Rotl7 a) = printf "(%s <<< 7)" a
algMapsDisplay (Rotl9 a) = printf "(%s <<< 9)" a
algMapsDisplay (Rotl13 a) = printf "(%s <<< 13)" a
algMapsDisplay (Rotl18 a) = printf "(%s <<< 18)" a
algMapsDisplay (a `Xor2` b) = printf "%s ⊕ %s" a b

-- |The F-algebra maps to build equations.
algMapsEquation :: ExprFT String -> String
algMapsEquation (ConstT i) = printf "%s" i
algMapsEquation (a `ModT` b) = printf "(%s + %s)" a b
algMapsEquation (Rotl7T a) = printf "(%s <<< 7)" a
algMapsEquation (Rotl9T a) = printf "(%s <<< 9)" a
algMapsEquation (Rotl13T a) = printf "(%s <<< 13)" a
algMapsEquation (Rotl18T a) = printf "(%s <<< 18)" a
algMapsEquation (a `Xor2T` b) = printf "%s ⊕ %s" a b

-- |The quarterround evaluator that will do the computation.
evalCompute :: Fix ExprF -> Word32
evalCompute = cata algMapsCompute

-- |The quarterround evaluator that will display expressions.
evalDisplay :: Fix ExprF -> String
evalDisplay = cata algMapsDisplay

-- |The quarterround evaluator that will build equations.
evalTChecker:: Fix ExprFT -> String
evalTChecker = cata algMapsEquation

-- |The right hand side of the `z1` expression as an expression. `((y0 + y3) <<< 7)`
rhs1 :: [Word32] -> Fix ExprF
rhs1 [y0, _, _, y3] = In $ Rotl7 (In $ In (Const y0) `Mod` In (Const y3))
rhs1 _ = In (Const 0)

-- |The right hand side of the `z1` expression as an equation. `((y0 + y3) <<< 7)`
rhs1T :: [String] -> Fix ExprFT
rhs1T [y0, _, _, y3] = In $ Rotl7T (In $ In (ConstT y0) `ModT` In (ConstT y3))
rhs1T _ = In (ConstT "0")

-- |The `z1` expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
z1 :: [Word32] -> Fix ExprF
z1 [y0, y1, y2, y3] = In $ In (Const y1) `Xor2` rhs1 [y0, y1, y2, y3]
z1 _ = In (Const 0)

-- |The `z1` equation. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
z1T :: [String] -> Fix ExprFT
z1T [y0, y1, y2, y3] = In $ In (ConstT y1) `Xor2T` rhs1T [y0, y1, y2, y3]
z1T _ = In (ConstT "0")

-- |The right hand side of the `z2` expression as an expression. `((z1 + y0) <<< 9)`
rhs2 :: [Word32] -> Fix ExprF
rhs2 [y0, y1, y2, y3] = In $ Rotl9 (In $ z1 [y0, y1, y2, y3] `Mod` In (Const y0))
rhs2 _ = In (Const 0)

-- |The right hand side of the `z2` expression as an equation. `((z1 + y0) <<< 9)`
rhs2T :: [String] -> Fix ExprFT
rhs2T [y0, y1, y2, y3] = In $ Rotl9T (In $ z1T [y0, y1, y2, y3] `ModT` In (ConstT y0))
rhs2T _ = In (ConstT "0")

-- |The `z2` expression. `y2 ⊕ ((z1 + y0) <<< 9)`
z2 :: [Word32] -> Fix ExprF
z2 [y0, y1, y2, y3] = In $ In (Const y2) `Xor2` rhs2 [y0, y1, y2, y3]
z2 _ = In (Const 0)

-- |The `z2` equation. `y2 ⊕ ((z1 + y0) <<< 9)`
z2T :: [String] -> Fix ExprFT
z2T [y0, y1, y2, y3] = In $ In (ConstT y2) `Xor2T` rhs2T [y0, y1, y2, y3]
z2T _ = In (ConstT "0")

-- |The right hand side of the `z3` expression as an expression. `(z2 + z1) <<< 13)`
rhs3 :: [Word32] -> Fix ExprF
rhs3 [y0, y1, y2, y3] = In $ Rotl13 (In $ z2 [y0, y1, y2, y3]  `Mod` z1 [y0, y1, y2, y3])
rhs3 _ = In (Const 0)

-- |The right hand side of the `z3` expression as an equation. `(z2 + z1) <<< 13)`
rhs3T :: [String] -> Fix ExprFT
rhs3T [y0, y1, y2, y3] = In $ Rotl13T (In $ z2T [y0, y1, y2, y3]  `ModT` z1T [y0, y1, y2, y3])
rhs3T _ = In (ConstT "0")

-- |The `z3` expression. `y3 ⊕ ((z2 + z1) <<< 13)`
z3 :: [Word32] -> Fix ExprF
z3 [y0, y1, y2, y3] = In $ In (Const y3) `Xor2` rhs3 [y0, y1, y2, y3]
z3 _ = In (Const 0)

-- |The `z3` equation. `y3 ⊕ ((z2 + z1) <<< 13)`
z3T :: [String] -> Fix ExprFT
z3T [y0, y1, y2, y3] = In $ In (ConstT y3) `Xor2T` rhs3T [y0, y1, y2, y3]
z3T _ = In (ConstT "0")

-- |The right hand side of the `z0` expression as an expression. `((z3 + z2) <<< 18)`
rhs0 :: [Word32] -> Fix ExprF
rhs0 [y0, y1, y2, y3] = In $ Rotl18 (In $ z3 [y0, y1, y2, y3] `Mod` z2 [y0, y1, y2, y3])
rhs0 _ = In (Const 0)

-- |The right hand side of the `z0` expression as an equation. `((z3 + z2) <<< 18)`
rhs0T :: [String] -> Fix ExprFT
rhs0T [y0, y1, y2, y3] = In $ Rotl18T (In $ z3T [y0, y1, y2, y3] `ModT` z2T [y0, y1, y2, y3])
rhs0T _ = In (ConstT "0")

-- |The `z0` expression. `y0 ⊕ ((z3 + z2) <<< 18)`
z0 :: [Word32] -> Fix ExprF
z0 [y0, y1, y2, y3] = In $ In (Const y0) `Xor2` rhs0 [y0, y1, y2, y3]
z0 _ = In (Const 0)

-- |The `z0` equation. `y0 ⊕ ((z3 + z2) <<< 18)`
z0T :: [String] -> Fix ExprFT
z0T [y0, y1, y2, y3] = In $ In (ConstT y0) `Xor2T` rhs0T [y0, y1, y2, y3]
z0T _ = In (ConstT "0")

-- |The quarterround expression computed. `quarterround(y) = (z0, z1, z2, z3)`
quarterroundCompute :: [Word32] -> [Word32]
quarterroundCompute input = [evalCompute $ z0 input, evalCompute $ z1 input,
    evalCompute $ z2 input, evalCompute $ z3 input]

-- |The quarterround expression computed. `quarterround(y) = (z0, z1, z2, z3)`
quarterroundDisplay :: [Word32] -> [String]
quarterroundDisplay input = [evalDisplay $ z0 input, evalDisplay $ z1 input,
    evalDisplay $ z2 input, evalDisplay $ z3 input]

-- |The quarterround expression as a `String` for displaying expression purposes.
quarterroundTypeChecker :: [String] -> [String]
quarterroundTypeChecker input = [evalTChecker $ z0T input, evalTChecker $ z1T input, evalTChecker $ z2T input,
    evalTChecker $ z3T input]
