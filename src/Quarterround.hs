module Quarterround
    (
    get0,
    get1,
    get2,
    get3,
    quarterround
    ) where

import Data.Bits
import Data.Word

get0 :: (Word32, Word32, Word32, Word32) -> Word32
get0 (a, _, _, _) = a
get1 :: (Word32, Word32, Word32, Word32) -> Word32
get1 (_, a, _, _) = a
get2 :: (Word32, Word32, Word32, Word32) -> Word32
get2 (_, _, a, _) = a
get3 :: (Word32, Word32, Word32, Word32) -> Word32
get3 (_, _, _, a) = a

data ExprF a = Const Word32
        | Mod a a
        | Rotl7 a
        | Rotl9 a
        | Rotl13 a
        | Rotl18 a
        | Xor2 a a -- `2` needed to avoid ambiguity with Data.Bits.Xor

-- needed if no derive is done
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (left `Mod` right) = f left `Mod` f right
    fmap f (left `Xor2` right) = f left `Xor2` f right
    fmap f (Rotl7 a) = Rotl7 (f a)
    fmap f (Rotl9 a) = Rotl9 (f a)
    fmap f (Rotl13 a) = Rotl13 (f a)
    fmap f (Rotl18 a) = Rotl18 (f a)

newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

alg :: ExprF Word32 -> Word32
alg (Const i)   = i
alg (a `Mod` b) = a + b
alg (Rotl7 a) = rotate a 7
alg (Rotl9 a) = rotate a 9
alg (Rotl13 a) = rotate a 13
alg (Rotl18 a) = rotate a 18
alg (a `Xor2` b) = xor a b

eval :: Fix ExprF -> Word32
eval = cata alg

rhs1 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
rhs1 (y0, _, _, y3) = In $ 
    Rotl7 (In $ In (Const y0) `Mod` In (Const y3))

z1 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
z1 (y0, y1, y2, y3) = In $ 
  In (Const y1) `Xor2` rhs1 (y0, y1, y2, y3)

rhs2 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
rhs2 (y0, y1, y2, y3) = In $ 
    Rotl9 (In $ z1 (y0, y1, y2, y3) `Mod` In (Const y0))

z2 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
z2 (y0, y1, y2, y3) = In $ 
    In (Const y2) `Xor2` rhs2 (y0, y1, y2, y3)

rhs3 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
rhs3 (y0, y1, y2, y3) = In $ 
    Rotl13 (In $ z2 (y0, y1, y2, y3)  `Mod` z1 (y0, y1, y2, y3))

z3 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
z3 (y0, y1, y2, y3) = In $ 
    In (Const y3) `Xor2` rhs3 (y0, y1, y2, y3)

rhs0 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
rhs0 (y0, y1, y2, y3) = In $ 
    Rotl18 (In $ z3 (y0, y1, y2, y3) `Mod` z2 (y0, y1, y2, y3))

z0 :: (Word32, Word32, Word32, Word32) -> Fix ExprF
z0 (y0, y1, y2, y3) = In $ 
    In (Const y0) `Xor2` rhs0 (y0, y1, y2, y3)

quarterround :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterround (y0, y1, y2, y3) = (eval $ z0 (y0, y1, y2, y3), eval $ z1 (y0, y1, y2, y3), 
    eval $ z2 (y0, y1, y2, y3), eval $ z3 (y0, y1, y2, y3))
