{-|
Module      : Quarterround
Description : Implementation of the Salsa20 stream cipher quarterround expressions.
Copyright   : (c) Alfredo Garcia, 2023
License     : MIT
Stability   : experimental
Portability : POSIX

This module provides the implementation of the quarterround function, a core element in the Salsa20 stream cipher.
The quarterround function manipulates a 1x4 matrix, performing a series of modular arithmetic and bitwise operations,
including addition (`Mod`), rotation (`Rotl`), and bitwise XOR (`Xor`).
These operations are expressed as an F-Algebra, allowing the construction and evaluation of quarterround expressions.

The module offers functionalities to:

- Compute numeric values resulting from quarterround expressions.
- Generate string representations of quarterround expressions.
- Perform Keelung specific computations using the `UInt 32` type.

-}
{-# LANGUAGE DataKinds #-}
{-@ LIQUID "--no-positivity-check" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--no-termination" @-}

module Quarterround
    (
        quarterroundCompute, quarterroundDisplay, quarterroundKeelung,
    )
where

import Data.Bits
import Data.Word
import Text.Printf
import Data.Either

import Keelung hiding (input, eq)

-- |The quarterround endofunctor to compute a Haskell `Word32` or a Haskell `String` type.
data ExprF a = Const (Either Word32 String)
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- |The quarterround endofunctor to compute a Keelung `UInt 32` type.
data ExprFKeelung a = ConstK (UInt 32)
        | ModK a a
        | Rotl7K a
        | Rotl9K a
        | Rotl13K a
        | Rotl18K a
        | XorK a a

-- |Quarterround functor instance for computing a Haskell value or displaying a Haskell string.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (left `Mod` right) = f left `Mod` f right
    fmap f (left `Xor2` right) = f left `Xor2` f right
    fmap f (Rotl7 a) = Rotl7 (f a)
    fmap f (Rotl9 a) = Rotl9 (f a)
    fmap f (Rotl13 a) = Rotl13 (f a)
    fmap f (Rotl18 a) = Rotl18 (f a)

-- |Quarterround functor instance for computing a Keelung type.
instance Functor ExprFKeelung where
    fmap _ (ConstK i) = ConstK i
    fmap f (left `ModK` right) = f left `ModK` f right
    fmap f (left `XorK` right) = f left `XorK` f right
    fmap f (Rotl7K a) = Rotl7K (f a)
    fmap f (Rotl9K a) = Rotl9K (f a)
    fmap f (Rotl13K a) = Rotl13K (f a)
    fmap f (Rotl18K a) = Rotl18K (f a)

-- |Fix and unFix.
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- |The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

-- |The F-algebra maps for a `Word32` evaluator.
algMapsCompute :: ExprF Word32 -> Word32
algMapsCompute (Const i) = fromLeft 0 i
algMapsCompute (a `Mod` b) = a + b
algMapsCompute (Rotl7 a) = Data.Bits.rotate a 7
algMapsCompute (Rotl9 a) = Data.Bits.rotate a 9
algMapsCompute (Rotl13 a) = Data.Bits.rotate a 13
algMapsCompute (Rotl18 a) = Data.Bits.rotate a 18
algMapsCompute (a `Xor2` b) = xor a b

-- |The F-algebra maps for a `String` evaluator.
algMapsDisplay :: ExprF String -> String
algMapsDisplay (Const i) = printf "%s" (fromRight "" i)
algMapsDisplay (a `Mod` b) = printf "(%s + %s)" a b
algMapsDisplay (Rotl7 a) = printf "(%s <<< 7)" a
algMapsDisplay (Rotl9 a) = printf "(%s <<< 9)" a
algMapsDisplay (Rotl13 a) = printf "(%s <<< 13)" a
algMapsDisplay (Rotl18 a) = printf "(%s <<< 18)" a
algMapsDisplay (a `Xor2` b) = printf "%s ⊕ %s" a b

-- |The F-algebra maps for a `UInt 32` Keelung evaluator.
algMapsKeelung :: ExprFKeelung (UInt 32) -> UInt 32
algMapsKeelung (ConstK i) = i
algMapsKeelung (a `ModK` b) = Keelung.AddU a b
algMapsKeelung (Rotl7K a) = Keelung.rotate a 7
algMapsKeelung (Rotl9K a) = Keelung.rotate a 9
algMapsKeelung (Rotl13K a) = Keelung.rotate a 13
algMapsKeelung (Rotl18K a) = Keelung.rotate a 18
algMapsKeelung (a `XorK` b) = Keelung.XorU a b

-- |The quarterround evaluator that will do the computation.
evalCompute :: Fix ExprF -> Word32
evalCompute = cata algMapsCompute

-- |The quarterround evaluator that will display equations.
evalDisplay :: Fix ExprF -> String
evalDisplay = cata algMapsDisplay

-- |The quarterround evaluator that will do the keelung computation.
evalKeelung :: Fix ExprFKeelung -> UInt 32
evalKeelung = cata algMapsKeelung

-- |The right hand side of the `z1` expression as an expression. `((y0 + y3) <<< 7)`
{-@ rhs1 :: {v:[_] | (len v) == 4 } -> _ @-}
rhs1 :: [Either Word32 String] -> Fix ExprF
rhs1 [y0, _, _, y3] = In $ Rotl7 (In $ In (Const y0) `Mod` In (Const y3))
rhs1 _ = error "input to `rhs1` must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z1` expression as a Keelung expression. `((y0 + y3) <<< 7)`
{-@ rhs1Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
rhs1Keelung :: [UInt 32] -> Fix ExprFKeelung
rhs1Keelung [y0, _, _, y3] = In $ Rotl7K (In $ In (ConstK y0) `ModK` In (ConstK y3))
rhs1Keelung _ = error "input to `rhs1Keelung` must be a list of 4 `UInt 32` numbers"

-- |The `z1` expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
{-@ z1 :: {v:[_] | (len v) == 4 } -> _ @-}
z1 :: [Either Word32 String] -> Fix ExprF
z1 [y0, y1, y2, y3] = In $ In (Const y1) `Xor2` rhs1 [y0, y1, y2, y3]
z1 _ = error "input to `z1` must be a list of 4 `Word32` numbers"

-- |The `z1` Keelung expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
{-@ z1Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
z1Keelung :: [UInt 32] -> Fix ExprFKeelung
z1Keelung [y0, y1, y2, y3] = In $ In (ConstK y1) `XorK` rhs1Keelung [y0, y1, y2, y3]
z1Keelung _ = error "input to `z1Keelung` must be a list of 4 `UInt 32` numbers"

-- |The right hand side of the `z2` expression as an expression. `((z1 + y0) <<< 9)`
{-@ rhs2 :: {v:[_] | (len v) == 4 } -> _ @-}
rhs2 :: [Either Word32 String] -> Fix ExprF
rhs2 [y0, y1, y2, y3] = In $ Rotl9 (In $ z1 [y0, y1, y2, y3] `Mod` In (Const y0))
rhs2 _ = error "input to `rhs2` must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z2` expression as a Keelung expression. `((z1 + y0) <<< 9)`
{-@ rhs2Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
rhs2Keelung :: [UInt 32] -> Fix ExprFKeelung
rhs2Keelung [y0, y1, y2, y3] = In $ Rotl9K (In $ z1Keelung [y0, y1, y2, y3] `ModK` In (ConstK y0))
rhs2Keelung _ = error "input to `rhs2Keelung` must be a list of 4 `UInt 32` numbers"

-- |The `z2` expression. `y2 ⊕ ((z1 + y0) <<< 9)`
{-@ z2 :: {v:[_] | (len v) == 4 } -> _ @-}
z2 :: [Either Word32 String] -> Fix ExprF
z2 [y0, y1, y2, y3] = In $ In (Const y2) `Xor2` rhs2 [y0, y1, y2, y3]
z2 _ = error "input to `z2` must be a list of 4 `Word32` numbers"

-- |The `z2` Keelung expression. `y2 ⊕ ((z1 + y0) <<< 9)`
{-@ z2Keelung :: {v:[_] | (len v) = 4 } -> _ @-}
z2Keelung :: [UInt 32] -> Fix ExprFKeelung
z2Keelung [y0, y1, y2, y3] = In $ In (ConstK y2) `XorK` rhs2Keelung [y0, y1, y2, y3]
z2Keelung _ = error "input to `z2Keelung` must be a list of 4 `UInt 32` numbers"

-- |The right hand side of the `z3` expression as an expression. `(z2 + z1) <<< 13)`
{-@ rhs3 :: {v:[_] | (len v) == 4 } -> _ @-}
rhs3 :: [Either Word32 String] -> Fix ExprF
rhs3 [y0, y1, y2, y3] = In $ Rotl13 (In $ z2 [y0, y1, y2, y3]  `Mod` z1 [y0, y1, y2, y3])
rhs3 _ = error "input to `rhs3` must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z3` expression as a Keelung expression. `(z2 + z1) <<< 13)`
{-@ rhs3Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
rhs3Keelung :: [UInt 32] -> Fix ExprFKeelung
rhs3Keelung [y0, y1, y2, y3] = In $ Rotl13K (In $ z2Keelung [y0, y1, y2, y3]  `ModK` z1Keelung [y0, y1, y2, y3])
rhs3Keelung _ = error "input to `rhs3Keelung` must be a list of 4 `UInt 32` numbers"

-- |The `z3` expression. `y3 ⊕ ((z2 + z1) <<< 13)`
{-@ z3 :: {v:[_] | (len v) == 4 } -> _ @-}
z3 :: [Either Word32 String] -> Fix ExprF
z3 [y0, y1, y2, y3] = In $ In (Const y3) `Xor2` rhs3 [y0, y1, y2, y3]
z3 _ = error "input to `z3` must be a list of 4 `Word32` numbers"

-- |The `z3` Keelung expression. `y3 ⊕ ((z2 + z1) <<< 13)`
{-@ z3Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
z3Keelung :: [UInt 32] -> Fix ExprFKeelung
z3Keelung [y0, y1, y2, y3] = In $ In (ConstK y3) `XorK` rhs3Keelung [y0, y1, y2, y3]
z3Keelung _ = error "input to `z3Keelung` must be a list of 4 `UInt 32` numbers"

-- |The right hand side of the `z0` expression as an expression. `((z3 + z2) <<< 18)`
{-@ rhs0 :: {v:[_] | (len v) == 4 } -> _ @-}
rhs0 :: [Either Word32 String] -> Fix ExprF
rhs0 [y0, y1, y2, y3] = In $ Rotl18 (In $ z3 [y0, y1, y2, y3] `Mod` z2 [y0, y1, y2, y3])
rhs0 _ = error "input to `rhs0`  must be a list of 4 `Word32` numbers"

-- |The right hand side of the `z0` expression a Keelung expression. `((z3 + z2) <<< 18)`
{-@ rhs0Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
rhs0Keelung :: [UInt 32] -> Fix ExprFKeelung
rhs0Keelung [y0, y1, y2, y3] = In $ Rotl18K (In $ z3Keelung [y0, y1, y2, y3] `ModK` z2Keelung [y0, y1, y2, y3])
rhs0Keelung _ = error "input to `rhs0Keelung`  must be a list of 4 `UInt 32` numbers"

-- |The `z0` expression. `y0 ⊕ ((z3 + z2) <<< 18)`
{-@ z0 :: {v:[_] | (len v) == 4 } -> _ @-}
z0 :: [Either Word32 String] -> Fix ExprF
z0 [y0, y1, y2, y3] = In $ In (Const y0) `Xor2` rhs0 [y0, y1, y2, y3]
z0 _ = error "input to `z0` must be a list of 4 `Word32` numbers"

-- |The `z0` Keelung expression. `y0 ⊕ ((z3 + z2) <<< 18)`
{-@ z0Keelung :: {v:[_] | (len v) == 4 } -> _ @-}
z0Keelung :: [UInt 32] -> Fix ExprFKeelung
z0Keelung [y0, y1, y2, y3] = In $ In (ConstK y0) `XorK` rhs0Keelung [y0, y1, y2, y3]
z0Keelung _ = error "input to `z0` must be a list of 4 `Word32` numbers"

-- |The quarterround expression computed.
{-@ quarterroundCompute :: {v:[_] | (len v) == 4 } -> {v:[_] | (len v) = 4 } @-}
quarterroundCompute :: [Word32] -> [Word32]
quarterroundCompute input@[_, _, _, _] = [
    evalCompute $ z0 $ map Left input,
    evalCompute $ z1 $ map Left input,
    evalCompute $ z2 $ map Left input,
    evalCompute $ z3 $ map Left input]
quarterroundCompute _ = error "input to `quarterroundCompute` must be a list of 4 `Word32` numbers"

-- |The quarterround expression as a string.
{-@ quarterroundDisplay :: {v:[_] | (len v) == 4 } -> {v:[_] | (len v) = 4 } @-}
quarterroundDisplay :: [String] -> [String]
quarterroundDisplay input@[_, _, _, _] = [
    evalDisplay $ z0 $ map Right input,
    evalDisplay $ z1 $ map Right input,
    evalDisplay $ z2 $ map Right input,
    evalDisplay $ z3 $ map Right input]
quarterroundDisplay _ = error "input to `quarterroundDisplay` must be a list of 4 `String` strings"

-- | The quarterround expression as a Keelung computation.
{-@ quarterroundKeelung :: {v:[_] | (len v) == 4 } -> _ @-}
quarterroundKeelung :: [UInt 32] -> Comp [UInt 32]
quarterroundKeelung input = do
    z1' <- reuse . evalKeelung . z1Keelung $ input
    z2' <- reuse . evalKeelung . z2Keelung $ input
    z3' <- reuse . evalKeelung . z3Keelung $ input
    z0' <- reuse . evalKeelung . z0Keelung $ input

    return [z0', z1', z2', z3']
