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

- The numeric value.
- The type string.

-}
module Quarterround
    (
    quarterroundCompute,
    quarterroundTypeChecker,
    ) where

import Data.Bits
import Data.Word
import Text.Printf

import Utils

-- |The quarterround endofunctor to compute a value and a string type.
data ExprF a = Const Word32 String
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- |Quarterround functor instance for computing a value and displaying a type string.
instance Functor ExprF where
    fmap _ (Const i name) = Const i name
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
algMapsCompute (Const i _) = i
algMapsCompute (a `Mod` b) = a + b
algMapsCompute (Rotl7 a) = rotate a 7
algMapsCompute (Rotl9 a) = rotate a 9
algMapsCompute (Rotl13 a) = rotate a 13
algMapsCompute (Rotl18 a) = rotate a 18
algMapsCompute (a `Xor2` b) = xor a b

-- |The F-algebra maps for `String` type checking.
algMapsEquation :: ExprF String -> String
algMapsEquation (Const _ name) = printf "%s" name
algMapsEquation (a `Mod` b) = printf "(%s + %s)" a b
algMapsEquation (Rotl7 a) = printf "(%s <<< 7)" a
algMapsEquation (Rotl9 a) = printf "(%s <<< 9)" a
algMapsEquation (Rotl13 a) = printf "(%s <<< 13)" a
algMapsEquation (Rotl18 a) = printf "(%s <<< 18)" a
algMapsEquation (a `Xor2` b) = printf "%s ⊕ %s" a b

-- |The quarterround evaluator that will do the computation.
evalCompute :: Fix ExprF -> Word32
evalCompute = cata algMapsCompute

-- |The quarterround evaluator that will build equations.
evalTChecker:: Fix ExprF -> String
evalTChecker = cata algMapsEquation

-- |The right hand side of the `z1` expression as an expression. `((y0 + y3) <<< 7)`
rhs1 :: [(Word32, String)] -> Fix ExprF
rhs1 [y0, _, _, y3] = In $ Rotl7 (In $ In (Const (fst y0) ("y0" ++ snd y0)) `Mod` In (Const (fst y3) ("y3" ++ snd y3)))
rhs1 _ = In (Const 0 "0")

-- |The `z1` expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
z1 :: [(Word32, String)] -> Fix ExprF
z1 [y0, y1, y2, y3] = In $ In (Const (fst y1) ("y1" ++ snd y1)) `Xor2` rhs1 [y0, y1, y2, y3]
z1 _ = In (Const 0 "0")

-- |The right hand side of the `z2` expression as an expression. `((z1 + y0) <<< 9)`
rhs2 :: [(Word32, String)] -> Fix ExprF
rhs2 [y0, y1, y2, y3] = In $ Rotl9 (In $ z1 [y0, y1, y2, y3] `Mod` In (Const (fst y0) ("y0" ++ snd y0)))
rhs2 _ = In (Const 0 "0")

-- |The `z2` expression. `y2 ⊕ ((z1 + y0) <<< 9)`
z2 :: [(Word32, String)] -> Fix ExprF
z2 [y0, y1, y2, y3] = In $ In (Const (fst y2) ("y2" ++ snd y2)) `Xor2` rhs2 [y0, y1, y2, y3]
z2 _ = In (Const 0 "0")

-- |The right hand side of the `z3` expression as an expression. `(z2 + z1) <<< 13)`
rhs3 :: [(Word32, String)] -> Fix ExprF
rhs3 [y0, y1, y2, y3] = In $ Rotl13 (In $ z2 [y0, y1, y2, y3]  `Mod` z1 [y0, y1, y2, y3])
rhs3 _ = In (Const 0 "0")

-- |The `z3` expression. `y3 ⊕ ((z2 + z1) <<< 13)`
z3 :: [(Word32, String)] -> Fix ExprF
z3 [y0, y1, y2, y3] = In $ In (Const (fst y3) ("y3" ++ snd y3)) `Xor2` rhs3 [y0, y1, y2, y3]
z3 _ = In (Const 0 "0")

-- |The right hand side of the `z0` expression as an expression. `((z3 + z2) <<< 18)`
rhs0 :: [(Word32, String)] -> Fix ExprF
rhs0 [y0, y1, y2, y3] = In $ Rotl18 (In $ z3 [y0, y1, y2, y3] `Mod` z2 [y0, y1, y2, y3])
rhs0 _ = In (Const 0 "0")

-- |The `z0` expression. `y0 ⊕ ((z3 + z2) <<< 18)`
z0 :: [(Word32, String)] -> Fix ExprF
z0 [y0, y1, y2, y3] = In $ In (Const (fst y0) ("y0" ++ snd y0)) `Xor2` rhs0 [y0, y1, y2, y3]
z0 _ = In (Const 0 "0")

-- |The quarterround expression computed. `quarterround(y) = (z0, z1, z2, z3)`
quarterroundCompute :: [Word32] -> [Word32]
quarterroundCompute input = [evalCompute $ z0 (listNumbersToPair input), evalCompute $ z1 (listNumbersToPair input),
    evalCompute $ z2 (listNumbersToPair input), evalCompute $ z3 (listNumbersToPair input)]

-- |The quarterround expression as a `String` for type checking purposes.
quarterroundGetStrings :: [(Word32, String)] -> [String]
quarterroundGetStrings input = [evalTChecker $ z0 input, evalTChecker $ z1 input, evalTChecker $ z2 input,
    evalTChecker $ z3 input]

quarterroundTypeChecker :: [(Word32, String)] -> [String]
quarterroundTypeChecker input = do 
    let quarterround_tc = quarterroundGetStrings input
    
    -- substitutions
    {-
    let r1 = T.pack (quarterround_tc !! 1)
    let r2 = T.replace r1 (T.pack "z1") (T.pack (quarterround_tc !! 2))
    let r3' = T.replace r1 (T.pack "z1") (T.pack (quarterround_tc !! 3))
    let r3 = T.replace r2 (T.pack "z2") r3'
    let r0' = T.replace r1 (T.pack "z1") (T.pack (head quarterround_tc))
    let r0'' = T.replace r2 (T.pack "z2") r0'
    let r0 = T.replace r3 (T.pack "z3") r0''

    [T.unpack r0, T.unpack r1, T.unpack r2, T.unpack r3]
    -}
    quarterround_tc

