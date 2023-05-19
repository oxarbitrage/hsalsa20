module Rowround
    (
    rowround
    ) where

import Types (VectorType, MatrixType)
import Quarterround

-- The endofunctor
data ExprF a = Const VectorType | Quarterround a

-- Functor instance. Needed if no automatic derive is done.
instance Functor ExprF where
    fmap _ (Const i) = Const i
    fmap f (Quarterround a) = Quarterround (f a)
    
-- Fix and unFix
newtype Fix f = In (f (Fix f))
unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- The catamorphism.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

-- The algebra maps.
alg :: ExprF VectorType -> VectorType
alg (Const i)   = i
alg (Quarterround a) = Quarterround.quarterround a

-- The evaluator.
eval :: Fix ExprF -> VectorType
eval = cata alg

-- The first quarterround expression.
quarterround1 :: MatrixType -> Fix ExprF
quarterround1 (a, _, _, _) = In $ 
    Quarterround (In $ Const a)

-- The second quarterround expression.
quarterround2 :: MatrixType -> Fix ExprF
quarterround2 (_, a, _, _) = In $ 
    Quarterround (In $ Const (sort2 a))

-- Sort a second input for rowround.
sort2 :: VectorType -> VectorType
sort2 (y4, y5, y6, y7) = (y5, y6, y7, y4) 

-- Inverse of `sort2`, used to order rowround output.
sort2_inv :: VectorType -> VectorType
sort2_inv (z5, z6, z7, z4) = (z4, z5, z6, z7) 

-- The third quarterround expression.
quarterround3 :: MatrixType -> Fix ExprF
quarterround3 (_, _, a, _) = In $ 
    Quarterround (In $ Const (sort3 a))

-- Sort a third input for rowround.
sort3 :: VectorType -> VectorType
sort3 (y8, y9, y10, y11) = (y10, y11, y8, y9) 

-- Inverse of `sort3`, used to order rowround output.
sort3_inv :: VectorType -> VectorType
sort3_inv (z10, z11, z8, z9) = (z8, z9, z10, z11) 

-- The fourth quarterround expression.
quarterround4 :: MatrixType -> Fix ExprF
quarterround4 (_, _, _, a) = In $ 
    Quarterround (In $ Const (sort4 a))

-- Sort a fourth input for rowround.
sort4 :: VectorType -> VectorType
sort4 (y12, y13, y14, y15) = (y15, y12, y13, y14) 

-- Inverse of `sort4`, used to order rowround output.
sort4_inv :: VectorType -> VectorType
sort4_inv (z15, z12, z13, z14) = (z12, z13, z14, z15)

-- The rowround expression.
rowround :: MatrixType -> MatrixType
rowround input = (eval $ quarterround1 input, sort2_inv(eval $ quarterround2 input), 
    sort3_inv(eval $ quarterround3 input), sort4_inv(eval $ quarterround4 input))
