module Quarterround
    (
    quarterround
    ) where

import Data.Bits
import Data.Word

import Utils (VectorType)

-- The endofunctor
data ExprF a = Const Word32
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- Functor instance. Needed if no automatic derive is done.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (left `Mod` right) = f left `Mod` f right
    fmap f (left `Xor2` right) = f left `Xor2` f right
    fmap f (Rotl7 a) = Rotl7 (f a)
    fmap f (Rotl9 a) = Rotl9 (f a)
    fmap f (Rotl13 a) = Rotl13 (f a)
    fmap f (Rotl18 a) = Rotl18 (f a)

-- Fix and unFix
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

-- The algebra maps.
alg :: ExprF Word32 -> Word32
alg (Const i)   = i
alg (a `Mod` b) = a + b
alg (Rotl7 a) = rotate a 7
alg (Rotl9 a) = rotate a 9
alg (Rotl13 a) = rotate a 13
alg (Rotl18 a) = rotate a 18
alg (a `Xor2` b) = xor a b

-- The evaluator.
eval :: Fix ExprF -> Word32
eval = cata alg

-- The right hand side of the `z1` expression as an expression. `((y0 + y3) <<< 7)`
rhs1 :: VectorType -> Fix ExprF
rhs1 (y0, _, _, y3) = In $ 
    Rotl7 (In $ In (Const y0) `Mod` In (Const y3))

-- The `z1` expression. `z1 = y1 ⊕ ((y0 + y3) <<< 7)`
z1 :: VectorType -> Fix ExprF
z1 (y0, y1, y2, y3) = In $ 
  In (Const y1) `Xor2` rhs1 (y0, y1, y2, y3)

-- The right hand side of the `z2` expression as an expression. `((z1 + y0) <<< 9)`
rhs2 :: VectorType -> Fix ExprF
rhs2 (y0, y1, y2, y3) = In $ 
    Rotl9 (In $ z1 (y0, y1, y2, y3) `Mod` In (Const y0))

-- The `z2` expression. `y2 ⊕ ((z1 + y0) <<< 9)`
z2 :: VectorType -> Fix ExprF
z2 (y0, y1, y2, y3) = In $ 
    In (Const y2) `Xor2` rhs2 (y0, y1, y2, y3)

-- The right hand side of the `z3` expression as an expression. `(z2 + z1) <<< 13)`
rhs3 :: VectorType -> Fix ExprF
rhs3 (y0, y1, y2, y3) = In $ 
    Rotl13 (In $ z2 (y0, y1, y2, y3)  `Mod` z1 (y0, y1, y2, y3))

-- The `z3` expression. `y3 ⊕ ((z2 + z1) <<< 13)`
z3 :: VectorType -> Fix ExprF
z3 (y0, y1, y2, y3) = In $ 
    In (Const y3) `Xor2` rhs3 (y0, y1, y2, y3)

-- The right hand side of the `z0` expression as an expression. `((z3 + z2) <<< 18)`
rhs0 :: VectorType -> Fix ExprF
rhs0 (y0, y1, y2, y3) = In $ 
    Rotl18 (In $ z3 (y0, y1, y2, y3) `Mod` z2 (y0, y1, y2, y3))

-- The `z0` expression. `y0 ⊕ ((z3 + z2) <<< 18)`
z0 :: VectorType -> Fix ExprF
z0 (y0, y1, y2, y3) = In $ 
    In (Const y0) `Xor2` rhs0 (y0, y1, y2, y3)

-- The quarterround expression. `quarterround(y) = (z0, z1, z2, z3)`
quarterround :: VectorType -> VectorType
quarterround (y0, y1, y2, y3) = (eval $ z0 (y0, y1, y2, y3), eval $ z1 (y0, y1, y2, y3), 
    eval $ z2 (y0, y1, y2, y3), eval $ z3 (y0, y1, y2, y3))
